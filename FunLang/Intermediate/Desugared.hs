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

children :: Desugared -> [Desugared]
children (Application cs _) = cs
children (Lambda _ body _) = [body]
children (Conditional conds alt _) = (alt : foldl (\list (a,b) -> (a:b:list)) [] conds)
children _ = []

freeVariables :: Desugared -> (Set.Set Identifier)
freeVariables (Lambda identifiers body _) =
    Set.difference (freeVariables body) (Set.fromList identifiers)
    
freeVariables (Id x _) = Set.singleton x

freeVariables node = Set.unions . map freeVariables $ children node


alphaSubstitute :: Map.Map Identifier Desugared -> Desugared -> Desugared
alphaSubstitute ids node =
    if Map.null ids then node else alphaSubstitute' ids node

alphaSubstitute' ids node@(Id identifier _) = 
    case (Map.lookup identifier ids) of
        Just rewrite -> rewrite
        Nothing -> node

alphaSubstitute' ids (Lambda identifiers body origin) =
    Lambda identifiers body' origin
    where
    body' = alphaSubstitute ids' body
    ids' = Map.difference ids (Map.fromList (map (\x -> (x, Id x)) identifiers))

alphaSubstitute' ids (Conditional conds alt origin) =
    Conditional conds' alt' origin
    where
    alt' = alphaSubstitute' ids alt
    conds' = map substitutePair conds
    substitutePair (a,b) = (alphaSubstitute' ids a, alphaSubstitute' ids b)
    
alphaSubstitute' ids (Application children origin) =
    Application (map (alphaSubstitute' ids) children) origin

alphaSubstitute' ids node = node

