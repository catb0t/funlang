module FunLang.Parser.Desugar where

import qualified Data.Map as Map

import FunLang.Parser.AST
import FunLang.Parser.Infix
import FunLang.Intermediate.Desugared
import FunLang.Intermediate.LetRec (letrec)

desugar :: () -> Expression -> Desugared
desugar symbols (InfixExpr first [] pos) = desugar symbols first
desugar symbols (InfixExpr first rest pos) =
    shunting_yard apply_fun (desugar symbols first) operands
    where
    apply_fun fun l r = Application [fun, l, r] (Source pos)
    operands = map dsg rest
    dsg ((op,oppos), operand) =
        case (Map.lookup op infix_operators) of 
            Just operator@(InfixOp identifier _ _) ->
                ((operator, Id identifier (Source oppos)), desugar symbols operand)
            Nothing -> error ("Can't find infix operator: " ++ op)


desugar symbols (ApplicationExpr children pos) =
    Application (map (desugar symbols) children) (Source pos)

desugar symbols (PrefixExpr (op,oppos) expr pos) =
    Application [(Id op (Source oppos)), (desugar symbols expr)] (Source pos)

desugar symbols (ConditionExpr conds alternative pos) =
    Conditional conds' (desugar symbols alternative) (Source pos)
    where
    conds' = map dsg conds
    dsg (a,b) = (desugar symbols a, desugar symbols b)
    
desugar symbols (LetExpr bindings expr pos) =
    letrec (Source pos) (zip args values) body
    where
    args = map (fst . fst) bindings
    values = map (desugar symbols . snd) bindings
    body = desugar symbols expr
        
desugar symbols (LambdaExpr args expr pos) =
    Lambda (map fst args) (desugar symbols expr) (Source pos)

desugar symbols (ConstantExpr value pos) = Constant value (Source pos)

desugar symbols (IdExpr (name, pos)) = Id name (Source pos)





