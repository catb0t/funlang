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
    Application (body':values) (Synthetic "Simple Let application" [])
    where
    (args,values) = unzip decls
    body' = (Lambda args body (Synthetic "Simple Let body" []))


letselect1 :: [Identifier] -> Set.Set Identifier -> Desugared -> Identifier -> (Desugared, Maybe (Identifier,[Identifier],Desugared,Desugared))
letselect1 locals dependencies definition decl =
    if null localdeps then (definition, Nothing)
    else
        let sel = selector localdeps
        in (sel, Just (decl, localdeps, definition, sel))
    where
    localdeps = [loc | loc <- locals, Set.member loc dependencies]
    selector deps =
        Application (
            (Id decl (Synthetic "Let selector function" [])) :
            [Id x (Synthetic "Let selector parameter" []) | x <- deps] )
                (Synthetic "Let selector application" [])

rewrite :: Map.Map Identifier Desugared -> Map.Map Identifier (Set.Set Identifier) -> [Identifier] -> (Desugared -> Desugared) -> Identifier -> Desugared
rewrite definitions dependencies locals substitute local =
    if null localdeps then def
    else Lambda localdeps (substitute def) (Synthetic ("Rewritten Let definition for " ++ local) [])
    where
    def = fromJust (Map.lookup local definitions)
    deps = fromJust (Map.lookup local dependencies)
    localdeps = [loc | loc <- locals, Set.member loc deps]


letselect :: Map.Map Identifier Desugared -> Map.Map Identifier (Set.Set Identifier) -> [Identifier] -> [Identifier] -> ([Desugared],[Desugared])
letselect definitions dependencies locals decls = 
    (selectors, rewrites)
    where
    (selectors, maybedefs) = unzip (map select decls)
    select decl = letselect1 locals (fromJust (Map.lookup decl dependencies)) (fromJust (Map.lookup decl definitions)) decl
    defs = catMaybes maybedefs
    substitutions = Map.fromList [(decl, selector) | (decl, _, _, selector) <- defs]
    rewrites = map (rewrite definitions dependencies locals substitute) locals
    substitute def = alphaSubstitute substitutions def

letscope :: Map.Map Identifier Desugared -> Map.Map Identifier (Set.Set Identifier) -> Desugared -> ([Identifier],[Identifier]) -> Desugared
letscope _ _ body (_, []) = body

letscope definitions _ body ([], decls) =
    simplelet [(decl, fromJust (Map.lookup decl definitions)) | decl <- decls] body
    
letscope definitions dependencies body (locals, decls) = 
    wrapped
    where
    wrapped = Application (wrapper : rewrites) (Synthetic "Recursive Let scope application" [])
    wrapper = Lambda locals body' (Synthetic "Recursive Let scope wrapper" [])
    body' = Application (scopebody:selectors) (Synthetic "Recursive Let body application" [])
    scopebody = Lambda decls body (Synthetic "Recursive Let scope body" [])

    (selectors, rewrites) = letselect definitions dependencies locals decls

recursivelet :: Map.Map Identifier (Set.Set Identifier) -> Map.Map Identifier Desugared -> Desugared -> Desugared
recursivelet recursive definitions body =
    if Map.null recursive then body
    else foldl (letscope definitions dependencies) body scopelocals
    where
    dependencies = depends recursive
    scopelist = scopes dependencies
    (scopelocals, scopedefs) = foldr locals ([], Set.empty)  scopelist
    locals (decls, defs) (rest, defined) =
        ((loc,defs):rest, Set.union defined (Set.fromList (defs ++ decls)))
        where
        loc = [decl | decl <- decls, Set.notMember decl defined]

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

