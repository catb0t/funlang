module FunLang.Intermediate.LetRec where

import qualified Data.Map as Map
import qualified Data.Set as Set

import FunLang.Intermediate.Desugared

desugarLetRec :: [(Identifier, Desugared)] -> Map.Map Identifier (Set.Set Identifier)

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

desugarLetRec decls =
    recurse
    where
    locals = map fst decls
    localset = Set.fromList locals
    freeVars = map (freeVariables . snd) decls
    recurse = Map.fromList
        [(identifier, free) | (identifier, free) <- zip locals freeVars, not (Set.null (Set.intersection localset free))]
    depends = closure recurse

--        Set.unions [closure x vars | (x, Just vars) <- map (\x -> x, map (Map.lookup x recurse)) (Set.toList set), x /= identifier]

