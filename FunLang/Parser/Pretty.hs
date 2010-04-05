module FunLang.Parser.Pretty where

import Text.PrettyPrint

import FunLang.Parser.AST

pretty :: Expression -> Doc
pretty (InfixExpr first rest) =
    text "Infix" $$ nest 2 (parens (pretty first $$ prettytail))
    where
    prettytail = hsep (map prettyapply rest)
    prettyapply (op,operand) = text op $$ pretty operand

pretty (ApplicationExpr children) =
    text "Application" $$ nest 2 (brackets (vcat (map pretty children)))

pretty (PrefixExpr op operand) = text "Prefix" <+> text op <+> pretty operand

pretty (ConditionExpr conds alt) =
    text "Conditional" $$ parens (brackets (nest 2 (vcat prettyconds)) $$ nest 2 (pretty alt))
    where
    prettyconds = map prettycond conds
    prettycond (cond,cons) = parens (pretty cond $$ text "then" $$ pretty cons)

pretty (LetExpr decls body) =
    text "Let" $$ nest 2 prettydecls $$ text "in" $$ nest 2 (pretty body)
    where
    prettydecls = vcat (map prettydecl decls)
    prettydecl (identifier, def) = text identifier <+> equals <+> pretty def

pretty (LambdaExpr identifiers body) =
    text "Lambda" <+> brackets (hsep (map text identifiers)) $$ nest 2 (pretty body)

pretty (ConstantExpr value) = text "Constant" <+> integer value

pretty (IdExpr identifier) = text "Id" <+> text identifier

pprint :: Expression -> String
pprint = render . pretty

