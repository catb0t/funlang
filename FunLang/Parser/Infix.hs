module FunLang.Parser.Infix where

import qualified Data.Map as Map

data Associativity = Associative | LeftAssociative | RightAssociative | NonAssociative
    deriving (Show, Eq)

data InfixOperator = InfixOp Associativity Int
    deriving (Show, Eq)

data PrefixOperator = PrefixOp
    deriving (Show, Eq)

type Applicator node = node -> node -> node -> node
type Stack node = [(InfixOperator,node)]
type Queue node = [node]
type ShuntingYard node = (Stack node,Queue node)
type InfixApplication node = ((InfixOperator, node), node)

shunting_yard :: Applicator node -> node -> [InfixApplication node] -> node
shunting_yard apply_fun first rest =
    case result of 
        ([], (x:[])) -> x
        (stack, queue) -> error ("Shunting yard terminated") -- , stack: " ++ show stack ++ " queue: " ++ show queue)
    where
    result = popAll (foldl shunt ([], [first]) rest)
    
    popAll ([], queue) = ([], queue)
    popAll yard = popAll (pop yard)
    
    pop (((_, body):bottom), (r:l:queue)) =
        let application = apply_fun body l r
        in (bottom, application:queue)
    pop (stack, queue) = error ("Cannot pop shunting yard") -- (" ++ show stack ++ ", " ++ show queue ++ ")")
   
    shunt ([], queue) (op, operand) = ([op], operand : queue)
    shunt (stack@((InfixOp sAss sPrec, _):bottom), queue)
        (op@(InfixOp iAss iPrec, _), operand) =
        if ((iAss == Associative || iAss == LeftAssociative) && (iPrec <= sPrec)) ||
            (iAss == RightAssociative && iPrec < sPrec)
            then
                shunt (pop (stack, queue)) (op, operand)
            else
                (op:stack, operand:queue)
--    shunt (stack, queue) (op, operand) =
--        error ("Cannot shunt " ++ show stack ++ ", " ++ show queue ++ " (" ++ show op ++ ", " ++ show operand ++ ")")

