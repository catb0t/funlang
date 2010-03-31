module FunLang.Parser.Parser (parser) where

import Text.Parsec

import FunLang.Parser.Lexer
import FunLang.Parser.AST

atomic_expr =
    id_expr <|> parens expression <?> "atomic expression"
    where
    id_expr = do
        name <- identifier
        return (IdExpr name)
            <?> "identifier expression"

application = do
    children <- many1 atomic_expr <?> "function application"
    return (
        if null . tail $ children
            then head children
            else ApplicationExpr children)

prefix_expr = prefix_app <|> application <?> "prefix expression"
    where
    prefix_app = do
        op <- operator
        arg <- prefix_expr
        return (PrefixExpr op arg)
            <?> "prefix application"

infix_expr = do
    left <- prefix_expr
    rest <- many infixtail
    return (if null rest then left else InfixExpr left rest)
        <?> "infix expression"
    where
    infixtail = do
        op <- operator
        right <- prefix_expr
        return (op, right)
            <?> "infix application"

cond_expr = do
    reserved "if"
    conds <- sepBy1 cond (reserved "elif")
    reserved "else"
    alternative <- expression
    return (ConditionExpr conds alternative)
    where
    cond = do
        condition <- expression
        reserved "then"
        consequent <- expression
        return (condition, consequent)
            <?> "condition \"then\" consequent"

declaration = do
    name <- identifier
    expr <- (decl <|> fun_decl)
    return (name, expr)
        <?> "declaration"
    where
    decl = do
        reservedOp "="
        expr <- expression
        return expr
            <?> "simple declaration"
    fun_decl = do
        args <- arguments
        reservedOp "="
        expr <- expression
        return (LambdaExpr args expr)
            <?> "function declaration"
        
        
declarations = commaSep1 declaration <?> "declarations"

let_expr = do
    reserved "let"
    decls <- declarations
    reserved "in"
    expr <- expression
    return (LetExpr decls expr)
        <?> "let expression"

argument = identifier
arguments = many1 argument

lambda_expr = do
    reserved "lambda"
    args <- arguments
    reservedOp ":"
    expr <- expression
    return (LambdaExpr args expr) 
        <?> "lambda expression"

expression =
    infix_expr <|> cond_expr <|> let_expr <|> lambda_expr
        <?> "expression"

parser = do 
    whiteSpace
    expr <- expression
    eof
    return expr

