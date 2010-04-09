module FunLang.Intermediate.LetRec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe

import FunLang.Intermediate.Desugared
import FunLang.Intermediate.Reductions

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

simplelet :: Origin -> [(Identifier, Desugared)] -> Desugared -> Desugared
simplelet _ [] body = body
simplelet org decls body =
    Application (body':values) (Synthetic "Simple Let application" [org])
    where
    (args,values) = unzip decls
    body' = (Lambda args body (Synthetic "Simple Let body" [origin body]))


letselect1 :: Origin -> [Identifier] -> Set.Set Identifier -> Set.Set Identifier -> Desugared -> Identifier -> (Desugared, Maybe (Identifier,[Identifier],Desugared,Desugared))
letselect1 org locals declset dependencies definition decl =
    if null localdeps then  (definition, Nothing)
    else
        let sel = selector localdeps
        in (sel, Just (decl, localdeps, definition, sel))
    where
    localdeps = [loc | loc <- locals, Set.member loc declset]
    selector deps =
        Application (
            (Id decl (Synthetic "Let selector function" [])) :
            [Id x (Synthetic "Let selector parameter" []) | x <- deps] )
                (Synthetic "Let selector application" [origin definition])

rewrite :: Origin -> Map.Map Identifier Desugared -> Map.Map Identifier (Set.Set Identifier) -> [Identifier] -> (Desugared -> Desugared) -> Identifier -> Desugared
rewrite org definitions dependencies locals substitute local =
    case (Map.lookup local dependencies) of
        Nothing -> def
        Just deps ->
            Lambda localdeps (substitute def) (Synthetic ("Rewritten Let definition for " ++ local) [origin def])
            where
            localdeps = [loc | loc <- locals, Set.member loc deps]
    where
    def = fromJust (Map.lookup local definitions)


letselect :: Origin -> Map.Map Identifier Desugared -> Map.Map Identifier (Set.Set Identifier) -> [Identifier] -> [Identifier] -> ([Desugared],[Desugared])
letselect org definitions dependencies locals decls = 
    (selectors, rewrites)
    where
    (selectors, maybedefs) = unzip (map select decls)
    declset = Set.fromList decls
    select decl = letselect1 org locals declset (fromJust (Map.lookup decl dependencies)) (fromJust (Map.lookup decl definitions)) decl
    defs = catMaybes maybedefs
    substitutions = Map.fromList [(decl, selector) | (decl, _, _, selector) <- defs]
    rewrites = map (rewrite org definitions dependencies locals substitute) locals
    substitute def = alphaSubstitute substitutions def

letscope :: Origin -> Map.Map Identifier Desugared -> Map.Map Identifier (Set.Set Identifier) -> Desugared -> ([Identifier],[Identifier]) -> Desugared
letscope _ _ _ body (_, []) = body

letscope org definitions _ body ([], decls) =
    simplelet org [(decl, fromJust (Map.lookup decl definitions)) | decl <- decls] body
    
letscope org definitions dependencies body (locals, decls) = 
    wrapped
    where
    wrapped = Application (wrapper : rewrites) (Synthetic "Recursive Let scope application" [org])
    wrapper = Lambda locals body' (Synthetic "Recursive Let scope wrapper" [])
    body' = Application (scopebody:selectors) (Synthetic "Recursive Let body application" [])
    scopebody = Lambda decls body (Synthetic "Recursive Let scope body" [org, origin body])
    (selectors, rewrites) = letselect org definitions dependencies locals decls

recursivelet :: Origin -> Map.Map Identifier (Set.Set Identifier) -> Map.Map Identifier Desugared -> Desugared -> Desugared
recursivelet org recursive definitions body =
    if Map.null recursive then body
    else foldl reduce body scopelocals
    where
    reduce body (x,y) = letscope org definitions dependencies body (x, y)
    dependencies = depends recursive
    scopelist = scopes dependencies
    (scopelocals, scopedefs) = foldl locals ([], Set.empty)  scopelist
    locals (rest, defined) (decls, defs) =
        ((loc,defs):rest, Set.union defined (Set.fromList (defs ++ decls)))
        where
        loc = [decl | decl <- decls, Set.notMember decl defined]

letrec :: Origin -> [(Identifier, Desugared)] -> Desugared -> Desugared
letrec _ [] body = body
letrec org decls body =
    recursivelet org recursive definitions body' 
    where
    body' = simplelet org simple body
    recursive = recursiveDecls decls
    notsimple = Set.unions (Map.elems recursive)
    simple = [(identifier, def) | (identifier, def) <- decls, not (Set.member identifier notsimple)]
    definitions = Map.fromList [(identifier, def) | (identifier, def) <- decls]

