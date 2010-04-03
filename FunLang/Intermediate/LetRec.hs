module FunLang.Intermediate.LetRec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

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
    freeVars = map (freeVariables . snd) decls
    recurse = [(identifier, free) | (identifier, free) <- zip locals freeVars, not (Set.null (Set.intersection localset free))]

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

