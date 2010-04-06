module FunLang.Intermediate.Pretty where

import Text.PrettyPrint

import FunLang.Parser.Pretty (prettyPos)
import FunLang.Intermediate.Desugared

prettyOrigin :: Origin -> Doc
prettyOrigin (Source pos) = prettyPos pos
prettyOrigin (Synthetic org ancestors) = braces (text org <+> brackets (hsep (map prettyOrigin ancestors)))

pretty :: Desugared -> Doc
pretty (Application children origin) =
    text "Application" <+> prettyOrigin origin $$
        nest 2 (brackets (vcat (map pretty children)))
        
pretty (Lambda identifiers body origin) =
    text "Lambda" <+> prettyOrigin origin $$
        nest 2 (brackets (hsep (map text identifiers)) $$ pretty body)

pretty (Conditional cond cons alt origin) =
    text "Conditional" <+> prettyOrigin origin $$
        parens (brackets (nest 2 prettycond) $$ nest 2 (pretty alt))
    where
    prettycond = parens (pretty cond $$ text "then" $$ pretty cons)
pretty (Constant value origin) = text "Constant" <+> integer value <+> prettyOrigin origin
pretty (Id identifier origin) = text "Id" <+> text identifier <+> prettyOrigin origin

pprint :: Desugared -> String
pprint = render . pretty


