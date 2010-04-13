module FunLang.Intermediate.Reductions where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import FunLang.Intermediate.Desugared

subFreeVars :: Map.Map Identifier DesugarTree -> Map.Map Identifier (Set.Set Identifier)
subFreeVars = Map.map freeVariables


legalize :: Map.Map Identifier (Set.Set Identifier) -> DesugarTree -> DesugarTree
legalize free node@(Tree.Node (Lambda _ _) _) =
    if updatednode then node' else node
    where
    (updatednode, node') = legalize' free node
    legalize' free node@(Tree.Node (Lambda ids org) (body:[])) = 
        if not bupd && null legals
        then (False,node)
        else
            (True, Tree.Node (Lambda renamed (Synthetic "Rewritten argument names" [org])) [body''])
        where
        allFree = Set.unions (Map.elems free)
        illegals = [identifier | identifier <- ids, Set.member identifier allFree]
        legalname (forbidden,name) =
            if Set.member name forbidden
            then legalname (forbidden,('\'':name))
            else (Set.insert name forbidden, name)
        legals =
            snd $ foldl reduce (allFree, []) illegals
            where
            reduce (forb, leg) ill = let (forb',newname) = legalname (forb, ill) in (forb', leg ++ [newname])
        renames = Map.fromList (zip illegals legals)
        renamed =
            map rename ids
            where
            rename identifier =
                case (Map.lookup identifier renames) of
                    Just x -> x
                    Nothing -> identifier
        subs = Map.map (\x -> (Tree.Node (Id x (Synthetic "Rewritten argument name" [])) [])) renames
        body'' = alphaSubstitute subs body'
        (bupd, body') = legalize' free' body
        free' = Map.difference free (Map.fromList [(identifier, undefined) | identifier <- renamed]) -- undefined should be ok
    legalize' free node@(Tree.Node (Conditional org) (cond:cons:alt:[])) =
        if a || b || c
        then (True, Tree.Node (Conditional (Synthetic "Rewritten argument names" [org])) [cond',cons',alt'])
        else (False, node)
        where
        (a, cond') = legalize' free cond
        (b, cons') = legalize' free cons
        (c, alt') = legalize' free alt
    legalize' free node@(Tree.Node (Application org) children) =
        if (any id upd)
        then (True, Tree.Node (Application (Synthetic "Rewritten argument names" [org])) children')
        else (False, node)
        where
        (upd, children') = unzip $ map (legalize' free) children
    legalize' free node = (False, node) 
 
legalize _ _ = error "Cannot legalize non-lambda nodes" 


alphaSubstitute :: Map.Map Identifier DesugarTree -> DesugarTree -> DesugarTree
alphaSubstitute subs node =
    if Map.null subs then node else alphaSubstitute' subs node

alphaSubstitute' subs node@(Tree.Node (Id identifier _) []) = 
    case (Map.lookup identifier subs) of
        Just rewrite -> rewrite
        Nothing -> node

alphaSubstitute' subs lambda@(Tree.Node (Lambda _ _) _) =
    Tree.Node (Lambda identifiers' (Synthetic "Alpha substitute" [org'])) [body'']
    where
    (Tree.Node (Lambda identifiers' org') (body':[])) = legalize (subFreeVars subs) lambda
    body'' = alphaSubstitute subs' body'
    subs' = Map.difference subs (Map.fromList (map (\x -> (x, undefined)) identifiers'))

alphaSubstitute' subs (Tree.Node (Conditional org) (cond:cons:alt:[])) =
    Tree.Node (Conditional (Synthetic "Alpha substitute" [org])) [cond',cons',alt']
    where
    sub = alphaSubstitute' subs
    cond' = sub cond
    cons' = sub cons
    alt' = sub alt
    
alphaSubstitute' subs (Tree.Node (Application org) children) =
    Tree.Node (Application (Synthetic "Alpha substitute" [org])) (map (alphaSubstitute' subs) children)

alphaSubstitute' _ node = node

betaReduce :: Map.Map Identifier DesugarTree -> DesugarTree -> DesugarTree
betaReduce subs node@(Tree.Node (Lambda identifiers org) (body:[])) = 
    if Map.null subs
    then node
    else
        if null ids' then body'
        else Tree.Node (Lambda ids' (Synthetic "Beta reduce" [org])) [body']
    where
    ids' = [identifier | identifier <- identifiers, Map.notMember identifier subs]
    body' = alphaSubstitute subs body
    
    
betaReduce _ _ = error "Cannot beta-reduce non-lambda nodes"


