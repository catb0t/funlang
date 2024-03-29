module FunLang.Parser.AST where

import qualified Text.Parsec as Parsec

import FunLang.Parser.Infix (InfixOperator, PrefixOperator)

type SourcePos = Parsec.SourcePos
type Identifier = (String, SourcePos)

data Declaration = 
    SimpleDecl
    | InfixDecl InfixOperator
    | PrefixDecl PrefixOperator
    deriving Show

data Expression = 
    InfixExpr Expression [(Identifier, Expression)] SourcePos
    | ApplicationExpr [Expression] SourcePos
    | PrefixExpr Identifier Expression SourcePos
    | ConditionExpr [(Expression, Expression)] Expression SourcePos
    | LetExpr [(Identifier, Declaration, Expression)] Expression SourcePos
    | LambdaExpr [Identifier] Expression SourcePos
    | ConstantExpr Integer SourcePos
    | IdExpr Identifier
    deriving Show

position :: Expression -> SourcePos
position (InfixExpr _ _ pos) = pos
position (ApplicationExpr _ pos) = pos
position (PrefixExpr _ _ pos) = pos
position (ConditionExpr _ _ pos) = pos
position (LetExpr _ _ pos) = pos
position (LambdaExpr _ _ pos) = pos
position (ConstantExpr _ pos) = pos
position (IdExpr (_, pos)) = pos

