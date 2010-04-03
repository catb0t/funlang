module FunLang.Parser.Desugar where

import qualified Data.Map as Map

import FunLang.Parser.AST
import FunLang.Parser.Infix
import FunLang.Intermediate.Desugared

desugar symbols (InfixExpr first []) = desugar symbols first
desugar symbols (InfixExpr first rest) =
    shunting_yard apply_fun (desugar symbols first) operands
    where
    apply_fun fun l r = Application [fun, l, r]
    operands = map dsg rest
    dsg (op, operand) =
        case (Map.lookup op infix_operators) of 
            Just operator@(InfixOp identifier _ _) ->
                ((operator, Id identifier), desugar symbols operand)
            Nothing -> error ("Can't find infix operator: " ++ op)


desugar symbols (ApplicationExpr children) =
    Application (map (desugar symbols) children)

desugar symbols (PrefixExpr op expr) = Application [(Id op), (desugar symbols expr)]

desugar symbols (ConditionExpr conds alternative) =
    Conditional conds' (desugar symbols alternative)
    where
    conds' = map dsg conds
    dsg (a,b) = (desugar symbols a, desugar symbols b)
    
desugar symbols (LetExpr bindings expr) =
    Application ((Lambda args body):values)
    where
    args = map fst bindings
    body = desugar symbols expr
    values = map (desugar symbols . snd) bindings
        
desugar symbols (LambdaExpr args expr) = Lambda args (desugar symbols expr)

desugar symbols (ConstantExpr value) = Constant value

desugar symbols (IdExpr name) = Id name





