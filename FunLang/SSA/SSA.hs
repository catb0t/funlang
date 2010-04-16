module FunLang.SSA.SSA where

data Function =
    Function [String] [(String, BasicBlock)]
    deriving Show

data BasicBlock =
    BasicBlock [(String, Instruction)] Terminator
    deriving Show

data Instruction =
    Phi [(String,String)]
    | FunCall String [String]
    | Const Integer
    deriving Show

data Terminator =
    Jump String
    | Branch String String String
    | Return String
    deriving Show


