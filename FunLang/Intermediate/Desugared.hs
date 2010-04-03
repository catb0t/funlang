module FunLang.Intermediate.Desugared where

import qualified Data.Set as Set
import qualified Data.Map as Map

type Identifier = String

data Desugared =
    Application [Desugared]
    | Lambda [Identifier] Desugared
    | Conditional [(Desugared, Desugared)] Desugared
    | Constant Integer
    | Id Identifier
    deriving (Show,Eq)

children :: Desugared -> [Desugared]
children (Application cs) = cs
children (Lambda _ body) = [body]
children (Conditional conds alt) = (alt : foldl (\list (a,b) -> (a:b:list)) [] conds)
children _ = []

freeVariables :: Desugared -> (Set.Set Identifier)
freeVariables (Lambda identifiers body) =
    Set.difference (freeVariables body) (Set.fromList identifiers)
    
freeVariables (Id x) = Set.singleton x

freeVariables node = Set.unions . map freeVariables $ children node


alphaSubstitute :: Map.Map Identifier Desugared -> Desugared -> Desugared
alphaSubstitute ids node =
    if Map.null ids then node else alphaSubstitute' ids node

alphaSubstitute' ids node@(Id identifier) = 
    case (Map.lookup identifier ids) of
        Just rewrite -> rewrite
        Nothing -> node

alphaSubstitute' ids (Lambda identifiers body) =
    alphaSubstitute ids' body
    where
    ids' = Map.difference ids (Map.fromList (map (\x -> (x, Id x)) identifiers))

alphaSubstitute' ids (Conditional conds alt) =
    Conditional conds' alt'
    where
    alt' = alphaSubstitute' ids alt
    conds' = map substitutePair conds
    substitutePair (a,b) = (alphaSubstitute' ids a, alphaSubstitute' ids b)
    
alphaSubstitute' ids (Application children) =
    Application (map (alphaSubstitute' ids) children)

alphaSubstitute' ids node = node

