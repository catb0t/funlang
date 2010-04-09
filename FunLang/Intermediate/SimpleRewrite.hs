module FunLang.Intermediate.SimpleRewrite where

import qualified Data.Map as Map
import Data.Maybe

import FunLang.Intermediate.Desugared
import FunLang.Intermediate.Reductions

rewrite :: Desugared -> Desugared
rewrite (Application (child:[]) _) = rewrite child

rewrite (Application ((Application largs lorg):rargs) rorg) =
    rewrite $ Application args' (Synthetic "Nested applications" [lorg, rorg])
    where
    args' = (largs ++ rargs)

rewrite (Application list@(lambda@(Lambda identifiers body _):rest) org) =
    if Map.null subs
    then Application (map rewrite list) org
    else rewrite $ 
        if null rest'
        then applied
        else Application (applied:rest') (Synthetic "Rewritten lambda application" [org])
    where
    arity = length identifiers
    args = take arity rest
    rest' = drop arity rest
    count = countOccurences (take (length args) identifiers) body
    subs = Map.fromList $
        [(identifier, arg) | (identifier, arg) <- zip identifiers args, 1 >= (fromJust (Map.lookup identifier count))]
    args' = [arg | (identifier, arg) <- zip identifiers args, Map.notMember identifier subs]
    applied = Application (wrapper:args') (Synthetic "Prune application" [])
    wrapper = betaReduce subs lambda

rewrite (Lambda largs (Lambda rargs body rorg) lorg) =
    rewrite $ Lambda args' body (Synthetic "Nested lambdas" [lorg, rorg])
    where
    args' = largs ++ rargs

rewrite (Lambda identifiers body org) =
    Lambda identifiers body' org
    where
    body' = rewrite body

rewrite (Conditional cond cons alt org) =
    Conditional cond' cons' alt' org
    where
    cond' = rewrite cond
    cons' = rewrite cons
    alt' = rewrite alt

rewrite node = node

