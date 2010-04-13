module FunLang.Intermediate.Conditional where

import qualified Data.Tree as Tree

import FunLang.Intermediate.Desugared

conditional ((cond, cons):rest) alt org =
    Tree.Node (Conditional org) [cond, cons, conditional rest alt org]

conditional [] alt _ = alt


