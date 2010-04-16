module FunLang.Interpreter.Interpreter where

import qualified Data.Map as Map
import qualified Data.Tree as Tree

import FunLang.Intermediate.Desugared
import FunLang.Interpreter.Values

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


evaluate frames (Tree.Node (Application origin) children) =
    foldl apply_value fun args
    where
    (fun:args) = map (evaluate frames) children
    apply_value (FunctionValue function) arg = applicate function arg
    apply_value _ _ = error ("Cannot apply, left side not a function")
    
evaluate frames (Tree.Node (Lambda identifiers origin) (body:[])) =
    FunctionValue (ClosureFunction frames identifiers body)
    
evaluate frames (Tree.Node (Conditional origin) (cond:cons:alternative:[])) = 
    case (evaluate frames cond) of
        (IntegerValue x) ->
            if x /= 0
            then evaluate frames cons
            else evaluate frames alternative
        _ -> error "Wrong type in condition"
    
evaluate frames (Tree.Node (Constant value origin) []) =
    IntegerValue value
    
evaluate frames (Tree.Node (Id identifier origin) []) =
    case (lookupValue frames identifier) of
        Just value -> value
        Nothing -> error ("Unknown identifier: " ++ show identifier)

