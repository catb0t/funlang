module FunLang.Interpreter.Values where

import qualified Data.Map as Map

import FunLang.Parser.Desugar
import FunLang.Parser.AST (Identifier)

data Function =
    ClosureFunction [Frame] [Identifier] Desugared
    | BuiltInFunction Int ([Value] -> Value)
    | PartialApplication Function [Value]
   
instance Show Function where
    show (ClosureFunction _ identifiers body) = "Closure " ++ show identifiers ++ " " ++ show body
    show (BuiltInFunction x _) = "Built-In arity: " ++ show x
    show (PartialApplication fun args) = "Partial application " ++ show fun ++ " " ++ show args

data Value =
    IntegerValue Int
    | FunctionValue Function
    deriving Show

arity :: Function -> Int
arity (ClosureFunction _ identifiers _) = length identifiers
arity (BuiltInFunction x _) = x
arity (PartialApplication fun args) = arity fun - length args

type Frame = Map.Map Identifier Value

lookupValue :: [Frame] -> Identifier -> Maybe Value
lookupValue [] _ = Nothing
lookupValue (top:bottom) identifier = 
    case (Map.lookup identifier top) of
        Just value -> Just value
        Nothing -> lookupValue bottom identifier

