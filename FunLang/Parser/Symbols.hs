module FunLang.Parser.Symbols where

import qualified Data.Map as Map

import FunLang.Parser.AST
import FunLang.Parser.Infix (InfixOperator, PrefixOperator)

data Scope = Scope (Map.Map String InfixOperator) (Map.Map String PrefixOperator)
type Symbols = [Scope]

lookupInfix :: Symbols -> String -> Maybe InfixOperator
lookupInfix [] _ = Nothing
lookupInfix ((Scope ops _):rest) sym =
    case Map.lookup sym ops of
        Just op -> Just op
        Nothing -> lookupInfix rest sym
        
lookupPrefix :: Symbols -> String -> Maybe PrefixOperator
lookupPrefix [] _ = Nothing
lookupPrefix ((Scope _ ops):rest) sym =
    case Map.lookup sym ops of
        Just op -> Just op
        Nothing -> lookupPrefix rest sym

scopeFromDecls :: [((String, SourcePos), Declaration, Expression)] -> Scope
scopeFromDecls [] = Scope Map.empty Map.empty
scopeFromDecls decls =
    Scope infixOps prefixOps
    where
    infixOps = Map.fromList [(identifier, op) | ((identifier,_), (InfixDecl op), _) <- decls]
    prefixOps = Map.fromList [(identifier, op) | ((identifier,_), (PrefixDecl op), _) <- decls]

