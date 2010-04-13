module FunLang.Parser.Desugar where

import qualified Data.Map as Map
import qualified Data.Tree as Tree

import FunLang.Parser.AST
import FunLang.Parser.Infix
import FunLang.Parser.Symbols
import FunLang.Intermediate.Desugared
import FunLang.Intermediate.Conditional
import FunLang.Intermediate.LetRec (letrec)

desugar :: Symbols -> Expression -> DesugarTree
desugar symbols (InfixExpr first [] pos) = desugar symbols first
desugar symbols (InfixExpr first rest pos) =
    shunting_yard apply_fun (desugar symbols first) operands
    where
    apply_fun fun l r = Tree.Node (Application (Source pos)) [fun, l, r]
    operands = map dsg rest
    dsg ((op,oppos), operand) =
        case lookupInfix symbols op of 
            Just operator ->
                ((operator, Tree.Node (Id (op ++ "infix") (Source oppos)) []), desugar symbols operand)
            Nothing -> error ("Can't find infix operator: " ++ op)


desugar symbols (ApplicationExpr children pos) =
    Tree.Node (Application (Source pos)) (map (desugar symbols) children)

desugar symbols (PrefixExpr (op,oppos) expr pos) =
    case lookupPrefix symbols op of
        Just _ -> Tree.Node (Application (Source pos))
            [Tree.Node (Id (op ++ "prefix") (Source oppos)) [], (desugar symbols expr)]
        Nothing -> error ("Can't find prefix operator: " ++ op)

desugar symbols (ConditionExpr conds alternative pos) =
    conditional conds' (desugar symbols alternative) (Source pos)
    where
    conds' = map dsg conds
    dsg (a,b) = (desugar symbols a, desugar symbols b)
    
desugar symbols (LetExpr decls expr pos) =
    letrec (Source pos) (zip args values) body
    where
    symbols' = scopeFromDecls decls : symbols
    values = map (\(_,_,expr) -> desugar symbols' expr) decls
    args = map arg decls
        where
        arg ((identifier, _), SimpleDecl, _) = identifier
        arg ((identifier, _), (InfixDecl _), _) = identifier ++ "infix"
        arg ((identifier, _), (PrefixDecl _), _) = identifier ++ "prefix"
    body = desugar symbols' expr
        
desugar symbols (LambdaExpr args expr pos) =
    Tree.Node (Lambda (map fst args) (Source pos)) [desugar symbols expr]

desugar symbols (ConstantExpr value pos) =
    Tree.Node (Constant value (Source pos)) []

desugar symbols (IdExpr (name, pos)) =
    Tree.Node (Id name (Source pos)) []





