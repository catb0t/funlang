module FunLang.Parser.AST where

type Identifier = String
type Operator = String

data Expression = 
    InfixExpr Expression [(Operator, Expression)]
    | ApplicationExpr [Expression]
    | PrefixExpr Operator Expression 
    | ConditionExpr [(Expression, Expression)] Expression
    | LetExpr [(Identifier, Expression)] Expression
    | LambdaExpr [Identifier] Expression
    | IdExpr String
    deriving Show

