module FunLang.Compiler.Compiler (compile) where

import qualified Data.Maybe as Maybe
import qualified Data.Tree as Tree
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.ST
import Data.STRef

import FunLang.Intermediate.Desugared
import FunLang.SSA.SSA

data CompilerState = State {
    currentBlock :: String,
    instructions :: [(String, Instruction)], -- reversed
    blocks :: [(String, BasicBlock)], -- reversed
    labels :: Set.Set String,
    globals :: Set.Set String,
    locals :: Set.Set String,
    privates :: Map.Map String Function
    }

unique forbidden prefix index = 
    if Set.member ident forbidden
    then unique forbidden prefix (index+1)
    else ident
    where
    ident = prefix ++ ('#' : show index)

newLabel (_, st) prefix = do
    (State c i b labels g lo p) <- readSTRef st
    let lab = unique labels prefix 0
    writeSTRef st (State c i b (Set.insert lab labels) g lo p)
    return lab

newLocal (self, st) prefix = do
    (State c i b la globals locals p) <- readSTRef st
    let forbidden = Set.insert self (Set.union locals globals)
    let loc = unique forbidden prefix 0
    writeSTRef st (State c i b la globals (Set.insert loc locals) p)
    return loc

getGlobals (_, st) = do
    state <- readSTRef st
    return $ globals state

makeCompiler self globals locals = do
    let label = "entry"
    stateref <- newSTRef (State label [] [] (Set.singleton label) (Set.insert self globals) locals Map.empty)
    return (self, stateref)

emit (_, st) local insn = 
    modifySTRef st (\(State c i b la g lo p) -> State c ((local,insn):i) b la g lo p)

emitt (_, st) term next = 
    modifySTRef st (\(State c i b la g lo p) -> State next [] ((c, BasicBlock (reverse i) term):b) la g lo p)

addPrivate (_, st) ident fun =
    modifySTRef st (\(State c i b la g lo privates) -> State c i b la g lo (Map.insert ident fun privates))

buildFun' args (State [] [] blocks _ _ _ privates) =
    Function args (reverse blocks) privates

buildFun (_, st) args = do
    state <- readSTRef st
    return $ buildFun' args state

compile :: String -> Set.Set String -> [String] -> DesugarTree -> Function
compile self globals args body = runST (compileST self globals args body)

compileST :: String -> Set.Set String -> [String] -> DesugarTree -> ST s Function
compileST self globals args body =  do
    compiler <- makeCompiler self globals (Set.fromList args)
    valLoc <- compile' compiler body
    emitt compiler (Return valLoc) ""
    fun <- buildFun compiler args
    return fun
 
compile' compiler (Tree.Node (Application org) children) = do
    appLoc <- newLocal compiler "application"
    (fun:args) <- mapM (compile' compiler) children
    emit compiler appLoc (FunCall fun args)
    return appLoc

compile' compiler lambda@(Tree.Node (Lambda args org) (body:[])) = do
    funcGlob <- newLocal compiler "lambda"
    globals <- getGlobals compiler
    func <- compileST funcGlob globals (free ++ args) body
    addPrivate compiler funcGlob func
    if null free
        then return funcGlob
        else do
            partLoc <- newLocal compiler "partial_lambda"
            emit compiler partLoc (FunCall funcGlob free)
            return partLoc
    where
    free = Set.toList $ freeVariables lambda

compile' compiler (Tree.Node (Conditional org) (cond:cons:alt:[])) = do
    consLab <- newLabel compiler "consequent"
    altLab <- newLabel compiler "alternative"
    condLab <- newLabel compiler "conditional"
    valLoc <- newLocal compiler "cond_value"

    condLoc <- compile' compiler cond
    emitt compiler (Branch condLoc consLab altLab) consLab
    consLoc <- compile' compiler cons
    emitt compiler (Jump condLab) altLab
    altLoc <- compile' compiler alt
    emitt compiler (Jump condLab) condLab
    emit compiler valLoc (Phi [(consLab, consLoc), (altLab, altLoc)])
    return valLoc


compile' compiler (Tree.Node (Id identifier org) []) =
    return identifier

compile' compiler (Tree.Node (Constant value org) []) = do
    loc <- newLocal compiler ("const" ++ show value)
    emit compiler loc (Const value)
    return loc

