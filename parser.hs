module Main where

import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (haskellDef)
import System.Environment(getArgs)
import qualified Data.Map as Map

-- lexer


opChar = oneOf "+-/*=:"

keywords = ["if", "then", "elif", "else", "let", "in", "lambda"]

langdef =
    Token.LanguageDef
        "{-" "-}" "--" True  -- comments
        alphaNum (alphaNum <|> oneOf "_")   -- identifiers (start, rest)
        opChar opChar   -- operators (first, rest)
        keywords -- keywords
        ["=", ":"] -- reserved operators
        True -- case sensitive


lexer = Token.makeTokenParser langdef

whiteSpace = Token.whiteSpace lexer
identifier = Token.identifier lexer
parens = Token.parens lexer
operator = Token.operator lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
commaSep1 = Token.commaSep1 lexer

-- AST

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

-- parser

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

-- desugar

data Associativity = Associative | LeftAssociative | RightAssociative | NonAssociative
    deriving (Show, Eq)

data InfixOperator = InfixOp String Associativity Int
    deriving (Show, Eq)

infix_operators =
    Map.fromList . map getName $
        [
            InfixOp "*" Associative 2,
            InfixOp "+" Associative 1
        ]
    where
    getName op@(InfixOp name _ _) = (name,op)

data Desugared =
    Application Desugared [Desugared]
    | Lambda [Identifier] Desugared
    | Conditional [(Desugared, Desugared)] Desugared
    | Id Identifier
    deriving (Show,Eq)


--    foldl reduce (desugar first) rest
--    reduce base (op, operand) = Application (Id op) [base, desugar operand]

desugar (InfixExpr first []) = desugar first
desugar (InfixExpr first rest) =
    case shunting_yard of
        ([], (x:[])) -> x
        (stack, queue) -> error ""
    where
    
    shunting_yard =
        pop (foldl shunt yard operands)
        where
        operands = map dsg rest
        dsg (op, operand) =
            case (Map.lookup op infix_operators) of 
                Just operator -> (operator, desugar operand)
                Nothing -> error ("Can't find infix operator: " ++ op)
    
    pop ([], queue) = ([], queue)
    pop (((InfixOp opName _ _):bottom), (l:r:queue)) =
        let application = (Application (Id opName) [r, l])
        in pop (bottom, application:queue)
    pop (stack, queue) = error ("Cannot pop (" ++ show stack ++ ", " ++ show queue ++ ")")
   
    shunt :: ([InfixOperator],[Desugared]) -> (InfixOperator, Desugared) -> ([InfixOperator],[Desugared])
    shunt ([], queue) (op, operand) = ([op], operand : queue)
    shunt (stack@((InfixOp sName sAss sPrec):bottom), queue)
        (op@(InfixOp _ iAss iPrec), operand) =
        if ((iAss == Associative || iAss == LeftAssociative) && (iPrec <= sPrec)) ||
            (iAss == RightAssociative && iPrec < sPrec)
            then
                shunt (pop (stack, queue)) (op, operand)
            else
                (op:stack, operand:queue)
--    shunt (stack, queue) (op, operand) =
--        error ("Cannot shunt " ++ show stack ++ ", " ++ show queue ++ " (" ++ show op ++ ", " ++ show operand ++ ")")

    yard = ([],[desugar first]) :: ([InfixOperator],[Desugared])

--    shunt ((top:stack), queue) (op, operand) = 
--    shunt (stack,queue) (op,operands)= 

desugar (ApplicationExpr (fun:args)) =
    Application (desugar fun) (map desugar args)

desugar (PrefixExpr op expr) = Application (Id op) [(desugar expr)]

desugar (ConditionExpr conds alternative) =
    Conditional conds' (desugar alternative)
    where
    conds' = map dsg conds
    dsg (a,b) = (desugar a, desugar b)
    
desugar (LetExpr bindings expr) =
    Application (Lambda args body) values
    where
    args = map fst bindings
    body = desugar expr
    values = map (desugar . snd) bindings
        
desugar (LambdaExpr args expr) = Lambda args (desugar expr)
desugar (IdExpr name) = Id name



-- driver

main = do
    args <- getArgs
    print args
    loop
    where
    loop = do
        line <- getLine
        if null line
            then return ()
            else do
                case (parse parser "" line) of
                    Left err -> do
                        putStrLn "Error at "
                        print err
                    Right x -> do
                        print x
                        print (desugar x)
                loop

