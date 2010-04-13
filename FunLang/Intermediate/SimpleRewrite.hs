module FunLang.Intermediate.SimpleRewrite where

import qualified Data.Map as Map
import qualified Data.Tree as Tree
import Data.Maybe

import FunLang.Intermediate.Desugared
import FunLang.Intermediate.Reductions

rewrite :: DesugarTree -> DesugarTree
rewrite (Tree.Node (Application _) (child:[])) = rewrite child

rewrite (Tree.Node (Application rorg) ((Tree.Node (Application lorg) largs):rargs))=
    rewrite $ Tree.Node (Application (Synthetic "Nested applications" [lorg, rorg])) args'
    where
    args' = (largs ++ rargs)

rewrite (Tree.Node (Application org) list@(lambda@(Tree.Node (Lambda identifiers _) (body:[])):rest)) =
    if Map.null subs
    then Tree.Node (Application org) (map rewrite list)
    else rewrite $ 
        if null rest'
        then applied
        else Tree.Node (Application (Synthetic "Rewritten lambda application" [org])) (applied:rest')
    where
    arity = length identifiers
    args = take arity rest
    rest' = drop arity rest
    count = countOccurences (take (length args) identifiers) body
    subs = Map.fromList $
        [(identifier, arg) | (identifier, arg) <- zip identifiers args, 1 >= (fromJust (Map.lookup identifier count))]
    args' = [arg | (identifier, arg) <- zip identifiers args, Map.notMember identifier subs]
    applied = Tree.Node (Application (Synthetic "Prune application" [])) (wrapper:args')
    wrapper = betaReduce subs lambda

rewrite (Tree.Node (Lambda largs lorg) ((Tree.Node (Lambda rargs rorg) (body:[])) : [])) =
    rewrite $ Tree.Node (Lambda args' (Synthetic "Nested lambdas" [lorg, rorg])) [body]
    where
    args' = largs ++ rargs

rewrite (Tree.Node (Lambda identifiers org) (body:[])) =
    Tree.Node (Lambda identifiers org) [body']
    where
    body' = rewrite body

rewrite (Tree.Node (Conditional org) (cond:cons:alt:[])) =
    Tree.Node (Conditional org) [cond',cons',alt']
    where
    cond' = rewrite cond
    cons' = rewrite cons
    alt' = rewrite alt

rewrite node = node

