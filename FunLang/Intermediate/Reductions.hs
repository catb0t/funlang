module FunLang.Intermediate.Reductions where

import FunLang.Intermediate.Desugared

import qualified Data.Map as Map
import qualified Data.Set as Set

subFreeVars :: Map.Map Identifier Desugared -> Map.Map Identifier (Set.Set Identifier)
subFreeVars = Map.map freeVariables


legalize :: Map.Map Identifier (Set.Set Identifier) -> Desugared -> Desugared
legalize free node@(Lambda _ _ _) =
    if updatednode then node' else node
    where
    (updatednode, node') = legalize' free node
    legalize' free node@(Lambda ids body org) = 
        if not bupd && null legals
        then (False,node)
        else
            (True, Lambda renamed body'' (Synthetic "Rewritten argument names" [org]))
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
        subs = Map.map (\x -> Id x (Synthetic "Rewritten argument name" [])) renames
        body'' = alphaSubstitute subs body'
        (bupd, body') = legalize' free' body
        free' = Map.difference free (Map.fromList [(identifier, undefined) | identifier <- renamed]) -- undefined should be ok
    legalize' free node@(Conditional cond cons alt org) =
        if a || b || c
        then (True, Conditional cond' cons' alt' (Synthetic "Rewritten argument names" [org]))
        else (False, node)
        where
        (a, cond') = legalize' free cond
        (b, cons') = legalize' free cons
        (c, alt') = legalize' free alt
    legalize' free node@(Application children org) =
        if (any id upd)
        then (True, Application children' (Synthetic "Rewritten argument names" [org]))
        else (False, node)
        where
        (upd, children') = unzip $ map (legalize' free) children
    legalize' free node = (False, node) 
 
legalize _ _ = error "Cannot legalize non-lambda nodes" 


alphaSubstitute :: Map.Map Identifier Desugared -> Desugared -> Desugared
alphaSubstitute subs node =
    if Map.null subs then node else alphaSubstitute' subs node

alphaSubstitute' subs node@(Id identifier _) = 
    case (Map.lookup identifier subs) of
        Just rewrite -> rewrite
        Nothing -> node

alphaSubstitute' subs lambda@(Lambda _ _ _) =
    Lambda identifiers' body'' (Synthetic "Alpha substitute" [org'])
    where
    (Lambda identifiers' body' org') = legalize (subFreeVars subs) lambda
    body'' = alphaSubstitute subs' body'
    subs' = Map.difference subs (Map.fromList (map (\x -> (x, undefined)) identifiers'))

alphaSubstitute' subs (Conditional cond cons alt org) =
    Conditional cond' cons' alt' (Synthetic "Alpha substitute" [org])
    where
    sub = alphaSubstitute' subs
    cond' = sub cond
    cons' = sub cons
    alt' = sub alt
    
alphaSubstitute' subs (Application children org) =
    Application (map (alphaSubstitute' subs) children) (Synthetic "Alpha substitute" [org])

alphaSubstitute' _ node = node

betaReduce :: Map.Map Identifier Desugared -> Desugared -> Desugared
betaReduce subs node@(Lambda identifiers body org) = 
    if Map.null subs
    then node
    else
        if null ids' then body'
        else Lambda ids' body' (Synthetic "Beta reduce" [org])
    where
    ids' = [identifier | identifier <- identifiers, Map.notMember identifier subs]
    body' = alphaSubstitute subs body
    
    
betaReduce _ _ = error "Cannot beta-reduce non-lambda nodes"


