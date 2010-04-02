module FunLang.Parser.AST where

type Identifier = String

data Expression = 
    InfixExpr Expression [(Identifier, Expression)]
    | ApplicationExpr [Expression]
    | PrefixExpr Identifier Expression 
    | ConditionExpr [(Expression, Expression)] Expression
    | LetExpr [(Identifier, Expression)] Expression
    | LambdaExpr [Identifier] Expression
    | ConstantExpr Integer
    | IdExpr Identifier
    deriving Show

