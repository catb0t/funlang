module FunLang.Intermediate.LetRec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe

import FunLang.Intermediate.Desugared


closure :: Ord a => Map.Map a (Set.Set a) -> Set.Set a -> Set.Set a
closure recurse set =
    closure' Set.empty set
    where
    closure' bound set =
        Set.union set expand
        where
        list = Set.toList set
        unbound = [expand | (x, Just expand) <- zip list (map (\x -> Map.lookup x recurse) list), not (Set.member x bound)]
        expand = Set.unions (map (closure' (Set.union bound set)) unbound)        

recursiveDecls :: [(Identifier, Desugared)] -> Map.Map Identifier (Set.Set Identifier)
recursiveDecls decls =
    Map.fromList recurse
    where
    locals = map fst decls
    localset = Set.fromList locals
    freeVars = map (Set.intersection localset . freeVariables . snd) decls
    recurse = [(identifier, free) | (identifier, free) <- zip locals freeVars, not (Set.null free)]

depends :: Map.Map Identifier (Set.Set Identifier) -> Map.Map Identifier (Set.Set Identifier)
depends recurse = Map.map (closure recurse) recurse

scopes :: Map.Map Identifier (Set.Set Identifier) -> [([Identifier], [Identifier])]
scopes depmap =
    (List.sortBy smaller) . map listify . Map.toList $ scopemap
    where
    listify (set,x) = (Set.toList set, x)
    smaller (list1,_) (list2,_) = compare (length list1) (length list2)
    scopemap = foldl add Map.empty (Map.toList depmap)
    add scopemap (identifier,deps) =
        Map.alter alter deps scopemap
        where
        alter Nothing = Just [identifier]
        alter (Just list) = Just (identifier : list)

simplelet :: [(Identifier, Desugared)] -> Desugared -> Desugared
simplelet [] body = body
simplelet decls body =
    Application ((Lambda args body (Synthetic "Simple Let body" [])):values) (Synthetic "Simple Let application" [])
    where
    (args,values) = unzip decls

recursivelet :: Map.Map Identifier (Set.Set Identifier) -> Map.Map Identifier Desugared -> Desugared -> Desugared
recursivelet recursive definitions body =
    if Map.null recursive then body
    else recursivelet' Set.empty scopelist body
    where
    dependencies = depends recursive
    scopelist = scopes dependencies
    recursivelet' :: Set.Set Identifier -> [([Identifier],[Identifier])] -> Desugared -> Desugared 
    recursivelet' _ [] body = body
    recursivelet' defined ((scope,decls):rest) body =
        Application (Lambda (map fst rewrites) wrapper (Synthetic "Recursive Let scope" []): map snd rewrites) (Synthetic "Recursive Let application" [])
        where
        locals :: [Identifier]
        locals = [x | x <- scope, not (Set.member x defined)]
        localset = Set.fromList locals
        body' = recursivelet' (Set.union defined localset) rest body
--        wrapper = Lambda locals (Application (body':selectors))
        wrapper = Application (Lambda locals body' (Synthetic "Recursive let body" []) : selectors) (Synthetic "Recursive Let body application" [])
        localDeps dec =
            filter (\x -> Set.member x deps) locals
            where 
            deps = fromJust (Map.lookup dec dependencies)
        
        (selectors, rewrites) =
            (sel, rewrite (catMaybes maybedefs))
            where
            rewrite :: [(Identifier, [Identifier], Desugared)] -> [(Identifier, Desugared)]
            rewrite list =
                map (\(identifier, dep, def) ->
                    (identifier, Lambda dep (substitute def) (Synthetic "Recursive Let rewrite" []))) list
                where
                (rews,deps,defs) = unzip3 list
                synId = Synthetic "Recursive Let id" []
                apps = [Application ((Id decl synId):map (\d -> Id d synId) dep) (Synthetic "Recursive Let rewrite selector" []) | (decl, dep) <- zip rews deps]
                substitutions = Map.fromList (zip rews apps)
                substitute = alphaSubstitute substitutions

            (sel, maybedefs) = unzip $ map select decls
            select decl = 
                let def = fromJust (Map.lookup decl definitions)
                in case localDeps decl of
                    [] -> (def , Nothing)
                    deps ->
                        (app, Just (decl, deps, def)) 
                        where
                        synId = Synthetic "Recursive Let selector id" []
                        app = Application ((Id decl synId):map (\d -> Id d synId) deps) (Synthetic "Recursive Let selector" [])


letrec :: [(Identifier, Desugared)] -> Desugared -> Desugared
letrec [] body = body
letrec decls body =
    recursivelet recursive recDef body' 
    where
    body' = simplelet simple body
    recursive = recursiveDecls decls
    recSet = Map.keysSet recursive
    notsimple = Set.unions (Map.elems recursive)
    simple = [(identifier, def) | (identifier, def) <- decls, not (Set.member identifier notsimple)]
    recDef = Map.fromList [(identifier, def) | (identifier, def) <- decls, (Set.member identifier recSet)]

