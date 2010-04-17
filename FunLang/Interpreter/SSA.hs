module FunLang.Interpreter.SSA where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import FunLang.SSA.SSA
import FunLang.Interpreter.Values

lookupLocal locals = lookupValue (snd (unzip locals))
lookupGlobal globals locals ident =
    case lookupLocal locals ident of
        Just x -> Just x
        Nothing -> lookupValue globals ident

filterLocals (top:bottom) label = top:[(lab, defs) | (lab, defs) <- bottom, lab /= label]

simulate globals (Function args blocklist@((entry, entrybb):_) privates) values =
    simulateBB globals' [locals] blocks entry entrybb
    where
    globals' = frame : globals
    frame = Map.map (\fun@(Function args _ _) -> FunctionValue (ClosureFunction globals' args fun)) privates
    locals = ("", Map.fromList $ zip args values)
    blocks = Map.fromList blocklist

simulateBB globals locals blocks label (BasicBlock insns term) =
    simulateTerm globals (frame:locals) blocks term
    where
    frame = (label, foldl sim Map.empty insns)
    sim scope (name, insn) = Map.insert name (simulateInsn globals ((label, scope):locals) insn) scope

simulateInsn _ locals@(_ : bottom@((predecessor, _) : _)) (Phi choices) = 
    select choices
    where
    select [] = error "Invalid predecessor labels in Phi node"
    select ((label, name):rest) = 
        if predecessor == label
        then Maybe.fromJust $ lookupLocal bottom name
        else select rest


simulateInsn globals locals (FunCall fun args) = 
    case lookupGlobal globals locals fun of 
        Just (FunctionValue def) ->
            simulateApply def values
        Just _ -> error "Invalid function call"
        Nothing -> error ("Can't find function " ++ fun)
    where
    values = map (Maybe.fromJust . lookupGlobal globals locals) args

simulateInsn globals locals (Const value) = 
    IntegerValue value

simulateTerm globals locals blocks (Jump dest) =
    simulateBB globals (filterLocals locals dest) blocks dest (Maybe.fromJust $ Map.lookup dest blocks)

simulateTerm globals locals blocks (Branch cond cons alt) =
    case value of
        IntegerValue val ->
            simulateTerm globals locals blocks (Jump follower)
            where
            follower = if val /= 0 then cons else alt
        _ ->
            error "Invalid value in branch conditional"
    where
    value = Maybe.fromJust $ lookupLocal locals cond

simulateTerm globals locals blocks (Return value) =
    Maybe.fromJust $ lookupGlobal globals locals value

simulateApply fun values = 
    let ar = arity fun
    in case compare (length values) ar of
        EQ -> simulateApply' fun values 
        LT -> FunctionValue $ PartialApplication fun values
        GT ->
            case simulateApply' fun (take ar values) of
                FunctionValue fun' -> simulateApply fun' (drop ar values)
                _ -> error "Invalid type in application (too many arguments?)"
    where
    simulateApply' (ClosureFunction env args body) values = 
        simulate env body values

    simulateApply' (PartialApplication fun largs) rargs =
        simulateApply' fun (largs ++ rargs)

    simulateApply' (BuiltInFunction ar callback) args = 
        callback args

