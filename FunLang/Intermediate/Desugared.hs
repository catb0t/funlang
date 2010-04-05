module FunLang.Intermediate.Desugared where

import qualified Data.Set as Set
import qualified Data.Map as Map

import FunLang.Parser.AST (SourcePos)

type Identifier = String

data Origin = Source SourcePos | Synthetic String [Origin]
    deriving (Show,Eq)

data Desugared =
    Application [Desugared] Origin
    | Lambda [Identifier] Desugared Origin
    | Conditional [(Desugared, Desugared)] Desugared Origin
    | Constant Integer Origin
    | Id Identifier Origin
    deriving (Show,Eq)
    
origin :: Desugared -> Origin
origin (Application _ org) = org
origin (Lambda _ _ org) = org
origin (Conditional _ _ org) = org
origin (Constant _ org) = org
origin (Id _ org) = org

children :: Desugared -> [Desugared]
children (Application cs _) = cs
children (Lambda _ body _) = [body]
children (Conditional conds alt _) = (alt : foldl (\list (a,b) -> (a:b:list)) [] conds)
children _ = []

freeVariables :: Desugared -> (Set.Set Identifier)
freeVariables = Map.keysSet . countFree' Set.empty

countFree :: Desugared -> Map.Map Identifier Int
countFree = countFree' Set.empty

countFree' :: Set.Set Identifier -> Desugared -> Map.Map Identifier Int
countFree' bound (Lambda identifiers body _) =
    countFree' (Set.union bound (Set.fromList identifiers)) body
    
countFree' bound (Id x _) =
    if Set.member x bound
    then Map.empty
    else Map.singleton x 1

countFree' bound node =
    Map.unionsWith (+) . map (countFree' bound) $ children node


countOccurences :: [Identifier] -> Desugared  -> Map.Map Identifier Int
countOccurences identifiers = countOccurences' (Map.fromList $ zip identifiers [0,0..]) Set.empty

countOccurences' :: Map.Map Identifier Int -> Set.Set Identifier -> Desugared -> Map.Map Identifier Int
countOccurences' count bound (Lambda identifiers body _) =
    countOccurences' count (Set.union bound (Set.fromList identifiers)) body
    
countOccurences' count bound (Id x _) =
    if Set.member x bound
    then count
    else Map.adjust (+1) x count

countOccurences' count bound node =
    Map.unionWith (+) count childcount
    where
    childcount = Map.unionsWith (+) . map (countOccurences' zeros bound) $ children node
    zeros = Map.fromList [(identifier, 0) | identifier <- Map.keys count, Set.notMember identifier bound]


alphaSubstitute :: Map.Map Identifier Desugared -> Desugared -> Desugared
alphaSubstitute ids node =
    if Map.null ids then node else alphaSubstitute' ids node

alphaSubstitute' ids node@(Id identifier _) = 
    case (Map.lookup identifier ids) of
        Just rewrite -> rewrite
        Nothing -> node

alphaSubstitute' ids (Lambda identifiers body org) =
    Lambda identifiers body' (Synthetic "Alpha substitute" [org])
    where
    body' = alphaSubstitute ids' body
    ids' = Map.difference ids (Map.fromList (map (\x -> (x, Id x)) identifiers))

alphaSubstitute' ids (Conditional conds alt org) =
    Conditional conds' alt' (Synthetic "Alpha substitute" [org])
    where
    alt' = alphaSubstitute' ids alt
    conds' = map substitutePair conds
    substitutePair (a,b) = (alphaSubstitute' ids a, alphaSubstitute' ids b)
    
alphaSubstitute' ids (Application children org) =
    Application (map (alphaSubstitute' ids) children) (Synthetic "Alpha substitute" [org])

alphaSubstitute' ids node = node


betaReduce :: Map.Map Identifier Desugared -> Desugared -> Desugared
betaReduce subs node@(Lambda identifiers body org) = 
    if Map.null subs
    then node
    else
        if null ids' then body'
        else Lambda ids' body' (Synthetic "Beta reduce" [org])
    where
    ids' = [identifier | identifier <- identifiers, Map.notMember identifier subs]
    body' = alphaSubstitute subs body
    
betaReduce _ _ = error "Cannot beta-reduce non-lambda nodes"

