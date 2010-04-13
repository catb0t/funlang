module FunLang.Intermediate.Desugared where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import FunLang.Parser.AST (SourcePos)

type Identifier = String

data Origin = Source SourcePos | Synthetic String [Origin]
    deriving (Show,Eq)

data Desugared =
    Application Origin
    | Lambda [Identifier] Origin
    | Conditional Origin
    | Constant Integer Origin
    | Id Identifier Origin
    deriving (Show,Eq)
    
type DesugarTree = Tree.Tree Desugared

origin :: DesugarTree -> Origin
origin (Tree.Node (Application org) _) = org
origin (Tree.Node (Lambda _ org) _) = org
origin (Tree.Node (Conditional org) _) = org
origin (Tree.Node (Constant _ org) _) = org
origin (Tree.Node (Id _ org) _) = org


freeVariables :: DesugarTree -> (Set.Set Identifier)
freeVariables = Map.keysSet . countFree' Set.empty

countFree :: DesugarTree -> Map.Map Identifier Int
countFree = countFree' Set.empty

countFree' :: Set.Set Identifier -> DesugarTree -> Map.Map Identifier Int
countFree' bound (Tree.Node (Lambda identifiers _) (body:[])) =
    countFree' (Set.union bound (Set.fromList identifiers)) body
    
countFree' bound (Tree.Node (Id x _) []) =
    if Set.member x bound
    then Map.empty
    else Map.singleton x 1

countFree' bound node =
    Map.unionsWith (+) . map (countFree' bound) $ Tree.subForest node


countOccurences :: [Identifier] -> DesugarTree  -> Map.Map Identifier Int
countOccurences identifiers = countOccurences' (Map.fromList $ zip identifiers [0,0..]) Set.empty

countOccurences' :: Map.Map Identifier Int -> Set.Set Identifier -> DesugarTree -> Map.Map Identifier Int
countOccurences' count bound (Tree.Node (Lambda identifiers _) (body:[])) =
    countOccurences' count (Set.union bound (Set.fromList identifiers)) body
    
countOccurences' count bound (Tree.Node (Id x _) []) =
    if Set.member x bound
    then count
    else Map.adjust (+1) x count

countOccurences' count bound node =
    Map.unionWith (+) count childcount
    where
    childcount = Map.unionsWith (+) . map (countOccurences' zeros bound) $ Tree.subForest node
    zeros = Map.fromList [(identifier, 0) | identifier <- Map.keys count, Set.notMember identifier bound]



