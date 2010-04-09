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
    | Conditional Desugared Desugared Desugared Origin
    | Constant Integer Origin
    | Id Identifier Origin
    deriving (Show,Eq)
    
origin :: Desugared -> Origin
origin (Application _ org) = org
origin (Lambda _ _ org) = org
origin (Conditional _ _ _ org) = org
origin (Constant _ org) = org
origin (Id _ org) = org

children :: Desugared -> [Desugared]
children (Application cs _) = cs
children (Lambda _ body _) = [body]
children (Conditional cond cons alt _) = [cond,cons,alt]
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



