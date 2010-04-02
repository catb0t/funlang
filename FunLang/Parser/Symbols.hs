module FunLang.Parser.Symbols where

import qualified Data.Map as Map

import FunLang.Parser.AST (Identifier)

type Symbol = Int

data Scope = Scope (Map.Map Identifier Symbol) 

data SymbolTable = SymbolTable [Scope] [String]

newScope (SymbolTable stack symbols) identifiers = 
    SymbolTable ((Scope locals) : stack) (symbols ++ identifiers)
    where
    locals = Map.fromList (zip identifiers [(length symbols)..])

enterScope (SymbolTable stack symbols) = SymbolTable ((Scope Map.empty) : stack) symbols
leaveScope (SymbolTable (top:bottom) symbols) = SymbolTable bottom symbols

newSymbols (SymbolTable ((Scope locals) : bottom) symbols) syms =
    SymbolTable ((Scope newlocals) : bottom) (symbols ++ syms)
    where
    newlocals = Map.union locals (Map.fromList newsyms)
    newsyms = zip syms [(length symbols)..]

newSymbol state sym = newSymbols state [sym]

lookupScope (Scope locals) identifier = Map.lookup locals identifier

lookupScopes [] _ = Nothing
lookupScopes (top:bottom) identifier =
    case (lookupScope top identifier) of
        Just sym -> Just sym
        Nothing -> lookupScopes bottom identifier

lookupSymbol (SymbolTable scopes _) = lookupScopes scopes
    
