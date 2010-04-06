module FunLang.Interpreter.Interpreter where

import qualified Data.Map as Map

import FunLang.Intermediate.Desugared
import FunLang.Interpreter.Values

applicate' :: Function -> [Value] -> Value
applicate' (ClosureFunction frames identifiers body) args =
    if length identifiers /= length args
        then error "Cannot apply lambda function, incorrect arity"
        else
            evaluate (top:frames) body
            where
            top = Map.fromList (zip identifiers args)
            
applicate' (BuiltInFunction ar builtin) args =
    if ar /= length args
        then error "Cannot applicate built-in function, incorrect arity"
        else builtin args
applicate' (PartialApplication fun largs) rargs = applicate' fun (largs ++ rargs)


applicate :: Function -> Value -> Value
applicate (PartialApplication fun partargs) arg = 
    if arity fun == length args
        then applicate' fun args
        else FunctionValue (PartialApplication fun args)
    where
    args = partargs ++ [arg]

applicate fun arg = 
    if arity fun == 1
        then applicate' fun [arg]
        else FunctionValue (PartialApplication fun [arg])


evaluate :: [Frame] -> Desugared -> Value
evaluate frames (Application children origin) =
    foldl apply_value fun args
    where
    (fun:args) = map (evaluate frames) children
    apply_value (FunctionValue function) arg = applicate function arg
    apply_value _ _ = error ("Cannot apply, left side not a function")
    
evaluate frames (Lambda identifiers body origin) =
    FunctionValue (ClosureFunction frames identifiers body)
    
evaluate frames (Conditional cond cons alternative origin) = 
    case (evaluate frames cond) of
        (IntegerValue x) ->
            if x /= 0
            then evaluate frames cons
            else evaluate frames alternative
        _ -> error "Wrong type in condition"
    
evaluate frames (Constant value origin) = IntegerValue value
    
evaluate frames (Id identifier origin) =
    case (lookupValue frames identifier) of
        Just value -> value
        Nothing -> error ("Unknown identifier: " ++ show identifier)

