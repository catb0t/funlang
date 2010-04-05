module FunLang.Interpreter.Pretty where

import Text.PrettyPrint

import qualified FunLang.Intermediate.Pretty as PrettyDsg
import FunLang.Interpreter.Values

prettyFunction :: Function -> Doc
prettyFunction (ClosureFunction capture identifiers body) = 
    text "Closure" <+> brackets (hsep (map text identifiers)) $$ nest 2 (PrettyDsg.pretty body)
prettyFunction (BuiltInFunction arity _) = 
    text "BuiltIn arity" <+> equals <+> int arity
prettyFunction (PartialApplication fun values) =
    text "Partial" $$ nest 2 (parens (prettyFunction fun $$ vcat (map pretty values)))

pretty :: Value -> Doc
pretty (IntegerValue value) = text "Integer" <+> integer value
pretty (FunctionValue fun) = text "Function" $$ nest 2 (prettyFunction fun)
   
pprint :: Value -> String
pprint = render . pretty

