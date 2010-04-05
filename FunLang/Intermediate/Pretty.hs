module FunLang.Intermediate.Pretty where

import Text.PrettyPrint

import FunLang.Intermediate.Desugared

pretty :: Desugared -> Doc
pretty (Application children) = text "Application" $$ nest 2 (brackets (vcat (map pretty children)))
pretty (Lambda identifiers body) = text "Lambda" <+> brackets (hsep (map text identifiers)) $$ nest 2 (pretty body)
pretty (Conditional conds alt) =
    text "Conditional" $$ parens (brackets (nest 2 (vcat prettyconds)) $$ nest 2 (pretty alt))
    where
    prettyconds = map prettycond conds
    prettycond (cond,cons) = parens (pretty cond $$ text "then" $$ pretty cons)
pretty (Constant value) = text "Constant" <+> integer value
pretty (Id identifier) = text "Id" <+> text identifier

pprint :: Desugared -> String
pprint = render . pretty


