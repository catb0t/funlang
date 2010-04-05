module FunLang.Parser.Pretty where

import Text.PrettyPrint

import FunLang.Parser.AST

prettyPos :: SourcePos -> Doc
prettyPos pos = braces (text (show pos))

prettyId :: (String, SourcePos) -> Doc
prettyId (name, pos) = text name <+> prettyPos pos

pretty :: Expression -> Doc
pretty (InfixExpr first rest pos) =
    text "Infix" <+> prettyPos pos $$
        nest 2 (parens (pretty first $$ prettytail))
    where
    prettytail = vcat (map prettyapply rest)
    prettyapply (op,operand) = prettyId op $$ pretty operand

pretty (ApplicationExpr children pos) =
    text "Application" <+> prettyPos pos $$
        nest 2 (brackets (vcat (map pretty children)))

pretty (PrefixExpr op operand pos) =
    text "Prefix" <+> prettyPos pos $$
        prettyId op $$ pretty operand

pretty (ConditionExpr conds alt pos) =
    text "Conditional" <+> prettyPos pos $$
        parens (brackets (nest 2 (vcat prettyconds)) $$ nest 2 (pretty alt))
    where
    prettyconds = map prettycond conds
    prettycond (cond,cons) = parens (pretty cond $$ text "then" $$ pretty cons)

pretty (LetExpr decls body pos) =
    text "Let" <+> prettyPos pos $$
        nest 2 prettydecls $$ text "in" $$ nest 2 (pretty body)
    where
    prettydecls = vcat (map prettydecl decls)
    prettydecl (identifier, def) = prettyId identifier $$ nest 2 (equals $$ pretty def)

pretty (LambdaExpr identifiers body pos) =
    text "Lambda" <+> prettyPos pos $$
        nest 2 (brackets (vcat (map prettyId identifiers)) $$ pretty body)

pretty (ConstantExpr value pos) = text "Constant" <+> integer value <+> prettyPos pos

pretty (IdExpr identifier) = text "Id" <+> prettyId identifier

pprint :: Expression -> String
pprint = render . pretty

