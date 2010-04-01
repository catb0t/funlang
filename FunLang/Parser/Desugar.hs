module FunLang.Parser.Desugar where

import qualified Data.Map as Map

import FunLang.Parser.AST

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

desugar (InfixExpr first []) = desugar first
desugar (InfixExpr first rest) =
    case shunting_yard of
        ([], (x:[])) -> x
        (stack, queue) -> error ""
    where
    
    shunting_yard =
        popAll (foldl shunt yard operands)
        where
        operands = map dsg rest
        dsg (op, operand) =
            case (Map.lookup op infix_operators) of 
                Just operator -> (operator, desugar operand)
                Nothing -> error ("Can't find infix operator: " ++ op)
    
    popAll ([], queue) = ([], queue)
    popAll yard = popAll (pop yard)
    
    pop (((InfixOp opName _ _):bottom), (l:r:queue)) =
        let application = (Application (Id opName) [r, l])
        in (bottom, application:queue)
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




