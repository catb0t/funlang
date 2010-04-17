module FunLang.SSA.Pretty where

import qualified Data.Map as Map
import Text.PrettyPrint

import FunLang.SSA.SSA

prettyFun (Function args blocks privates) =
    text "Function" <+> brackets (hsep (map text args)) $$
        nest 2 (
            (vcat [text ident <+> equals $$ nest 2 (prettyFun fun) | (ident, fun) <- (Map.toList privates)]) $$
            (vcat [text label <+> colon $$ nest 2 (prettyBB bb) | (label, bb) <- blocks])
            )

prettyBB (BasicBlock insns term) =
    vcat [text local <+> equals <+> prettyInsn insn | (local, insn) <- insns] $$
    prettyTerm term

prettyInsn (Phi choices) = text "Phi" <+> text (show choices)
prettyInsn (FunCall fun args) = text fun <+> parens (hsep [text a | a <- args])
prettyInsn (Const value) = text (show value)
prettyTerm (Jump dest) = text "Jump" <+> text dest
prettyTerm (Branch cond cons alt) = text "Branch" <+> text cond <+> text cons <+> text alt
prettyTerm (Return value) = text "Return" <+> text value

pprint = render . prettyFun
