module FunLang.Parser.Parser (parser) where

import Text.Parsec

import FunLang.Parser.Lexer
import FunLang.Parser.AST

parse_id = do
    pos <- getPosition
    name <- identifier
    return (name, pos)
        <?> "identifier"
        
parse_op = do
    pos <- getPosition
    name <- operator
    return (name, pos)
        <?> "operator"

constant_expr = do
    pos <- getPosition
    value <- natural
    return (ConstantExpr value pos)
        <?> "constant"

atomic_expr =
    id_expr <|> constant_expr <|> parens expression <?> "atomic expression"
    where
    id_expr = do
        ident <- parse_id
        return (IdExpr ident)
            <?> "identifier expression"

application = do
    pos <- getPosition
    children <- many1 atomic_expr <?> "function application"
    return (
        if null . tail $ children
            then head children
            else ApplicationExpr children pos)

prefix_expr = prefix_app <|> application <?> "prefix expression"
    where
    prefix_app = do
        pos <- getPosition
        op <- operator
        arg <- prefix_expr
        return (PrefixExpr (op, pos) arg pos)
            <?> "prefix application"

infix_expr = do
    pos <- getPosition
    left <- prefix_expr
    rest <- many infixtail
    return (if null rest then left else InfixExpr left rest pos)
        <?> "infix expression"
    where
    infixtail = do
        pos <- getPosition
        op <- operator
        right <- prefix_expr
        return ((op,pos), right)
            <?> "infix application"

cond_expr = do
    pos <- getPosition
    reserved "if"
    conds <- sepBy1 cond (reserved "elif")
    reserved "else"
    alternative <- expression
    return (ConditionExpr conds alternative pos)
    where
    cond = do
        condition <- expression
        reserved "then"
        consequent <- expression
        return (condition, consequent)
            <?> "condition \"then\" consequent"

declaration = do
    pos <- getPosition
    name <- identifier
    expr <- (decl <|> fun_decl)
    return ((name,pos), expr)
        <?> "declaration"
    where
    decl = do
        reservedOp "="
        expr <- expression
        return expr
            <?> "simple declaration"
    fun_decl = do
        pos <- getPosition
        args <- arguments
        reservedOp "="
        expr <- expression
        return (LambdaExpr args expr pos)
            <?> "function declaration"
        
        
declarations = commaSep1 declaration <?> "declarations"

let_expr = do
    pos <- getPosition
    reserved "let"
    decls <- declarations
    reserved "in"
    expr <- expression
    return (LetExpr decls expr pos)
        <?> "let expression"

argument = do
    pos <- getPosition
    name <- identifier
    return (name, pos)
        <?> "argument"
arguments = many1 argument <?> "arguments"

lambda_expr = do
    pos <- getPosition
    reserved "lambda"
    args <- arguments
    reservedOp ":"
    expr <- expression
    return (LambdaExpr args expr pos) 
        <?> "lambda expression"

expression =
    infix_expr <|> cond_expr <|> let_expr <|> lambda_expr
        <?> "expression"

parser = do 
    whiteSpace
    expr <- expression
    eof
    return expr

