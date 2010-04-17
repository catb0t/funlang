module FunLang.Interpreter.Values where

import qualified Data.Map as Map

import FunLang.Intermediate.Desugared

data Function code =
    ClosureFunction [Frame code] [Identifier] code
    | BuiltInFunction Int ([Value code] -> (Value code))
    | PartialApplication (Function code) [Value code]
   
data Value code =
    IntegerValue Integer
    | FunctionValue (Function code)

instance Show (Value code) where
    show (IntegerValue value) = show value 
    show (FunctionValue fun) = "Function (arity: " ++ show (arity fun) ++ ")"

arity :: (Function code) -> Int
arity (ClosureFunction _ identifiers _) = length identifiers
arity (BuiltInFunction x _) = x
arity (PartialApplication fun args) = arity fun - length args

type Frame code = Map.Map String (Value code)

lookupValue :: [Frame code] -> Identifier -> Maybe (Value code)
lookupValue [] _ = Nothing
lookupValue (top:bottom) identifier = 
    case (Map.lookup identifier top) of
        Just value -> Just value
        Nothing -> lookupValue bottom identifier

