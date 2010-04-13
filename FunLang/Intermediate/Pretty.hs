module FunLang.Intermediate.Pretty where

import Text.PrettyPrint

import qualified Data.Tree as Tree

import FunLang.Parser.Pretty (prettyPos)
import FunLang.Intermediate.Desugared

prettyOrigin :: Origin -> Doc
prettyOrigin (Source pos) = prettyPos pos
prettyOrigin (Synthetic org ancestors) = braces (text org <+> brackets (hsep (map prettyOrigin ancestors)))

pretty :: DesugarTree -> Doc
pretty (Tree.Node (Application origin) children) =
    text "Application" <+> prettyOrigin origin $$
        nest 2 (brackets (vcat (map pretty children)))
        
pretty (Tree.Node (Lambda identifiers origin) (body:[])) =
    text "Lambda" <+> prettyOrigin origin $$
        nest 2 (brackets (hsep (map text identifiers)) $$ pretty body)

pretty (Tree.Node (Conditional origin) (cond:cons:alt:[])) =
    text "Conditional" <+> prettyOrigin origin $$
        parens (brackets (nest 2 prettycond) $$ nest 2 (pretty alt))
    where
    prettycond = parens (pretty cond $$ text "then" $$ pretty cons)
pretty (Tree.Node (Constant value origin) []) = text "Constant" <+> integer value <+> prettyOrigin origin
pretty (Tree.Node (Id identifier origin) []) = text "Id" <+> text identifier <+> prettyOrigin origin

pprint :: DesugarTree -> String
pprint = render . pretty


