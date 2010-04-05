module FunLang.Intermediate.SimpleRewrite where

import qualified Data.Map as Map
import Data.Maybe

import FunLang.Intermediate.Desugared

rewriteApplication app@(Application args org) = 
    if rewritten
    then rewrite' $ Application args' (Synthetic "Application rewrite" [org])
    else (False, app)
    where
    (updated, args') = unzip (map rewrite' args)
    rewritten = any id updated

rewrite :: Desugared -> Desugared
rewrite node =
    rew' (True,node)
    where
    rew' (False, node) = node
    rew' (True, node) = rew' (rewrite' node)

rewrite' :: Desugared -> (Bool, Desugared)
rewrite' (Application (child:[]) _) = (True, child)
rewrite' (Application ((Application largs lorg):(Application rargs rorg):rest) org) =
    rewrite' $ Application (largs ++ (rargs ++ rest)) (Synthetic "Combined applications" [org, lorg, rorg])
rewrite' (Application ((Application largs lorg):rargs) rorg) =
    rewrite' $ Application (largs ++ rargs) (Synthetic "Nested applications" [lorg, rorg])

rewrite' node@(Application (lambda@(Lambda identifiers body _):rest) org) =
    if Map.null subs
    then rewriteApplication node
    else rewrite' $ 
        if null rest'
        then applied
        else Application (applied:rest') (Synthetic "Rewritten lambda application" [org])
    where
    arity = length identifiers
    args = take arity rest
    rest' = drop arity rest
    count = countOccurences (take (length args) identifiers) body
    subs = Map.fromList $
        [(identifier, arg) | (identifier, arg) <- zip identifiers args, 2 >= (fromJust (Map.lookup identifier count))]
    args' = [arg | (identifier, arg) <- zip identifiers args, Map.notMember identifier subs]
    applied = Application (wrapper:args') (Synthetic "Prune application" [])
    wrapper = betaReduce subs lambda

rewrite' node@(Lambda identifiers body org) =
    if updated
    then (True, Lambda identifiers body' (Synthetic "Rewritten lambda" []))
    else (False, node)
    where
    (updated, body') = rewrite' body

rewrite' node = (False, node)

