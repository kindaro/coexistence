#! /usr/bin/env stack
{- stack
    script
        --resolver lts-9.0
        --package fgl
        --package mtl
        --package set-monad
        --package containers
 -}

module Coexistence where

import Control.Monad.Identity
import Data.Graph.Inductive
import qualified Data.List ((\\))
import Data.List (zipWith4)
import qualified Data.Set.Monad as Set
import Data.Set.Monad (Set, member, insert, delete)
import Data.Foldable (foldl')
import Data.Tree (Tree(Node), drawTree)
import Data.Maybe (fromJust)

data Actor = Us | Cat | Nina | Pasha | Tenant
    deriving (Eq, Ord, Show)

data Event = Enter Actor | Exit Actor | None
    deriving (Eq, Ord, Show)

type Timeline = Gr Event ()

type Stages = [Timeline]

example :: Timeline
example = buildGr $ reverse
    [ ([], 1, Enter Us, [])
    , ([((), 1)], 2, Enter Cat, [])
    , ([((), 1)], 3, Enter Pasha, [])
    , ([((), 2)], 6, Exit Cat, [])
    , ([((), 3), ((), 2)], 4, Enter Nina, [])
    , ([((), 3)], 7, Exit Pasha, [])
    , ([((), 7)], 5, Enter Tenant, [])
    , ([((), 5)], 8, Exit Us, [])
    ]

main = do
    print example
    print $ reachable 2 example
    print $ reachable 2 (grev example)
    print $ tord 2 example
    print $ example \\ tord 2 example
    putStrLn ""
    print $ stagePoints example
    putStrLn ""
    let graphs = stages example
    let trees = tree <$> graphs
    let stages = fold <$> trees
    let prettyGraphs = prettify <$> graphs
    let prettyTrees = drawTree . fmap show <$> trees

    let zipped = zipWith4 (\g t s p -> print g >> putStrLn t >> print s >> putStrLn p >> putStrLn "") graphs prettyTrees stages prettyGraphs

    sequence zipped


difference :: (DynGraph gr) => gr a b -> gr a b -> gr a b
difference x y = subgraph (nodes x Data.List.\\ nodes y) x

(\\) :: (DynGraph gr) => gr a b -> gr a b -> gr a b
(\\) = difference
infix 5 \\

reachable' :: (DynGraph gr) => Node -> gr a b -> Set Node
reachable' node = Set.fromList . reachable node
nodes' :: Graph gr => gr a b -> Set Node
nodes' = Set.fromList . nodes
subgraph' nodes = subgraph (Set.elems nodes)
dff'' nodes graph = dff (Set.elems nodes) graph

tord :: (DynGraph gr) => Node -> gr a b -> gr a b -- ^ Totally ordered subgraph at given node.
tord node graph = subgraph' nodes graph
    where
    nodes :: Set Node
    nodes = reachable' node graph `mappend` reachable' node (grev graph)

inde :: (DynGraph gr) => Node -> gr a b -> gr a b
inde node graph = graph \\ tord node graph

stages :: Timeline -> Stages
stages graph = (`graphBefore` graph) <$> (Set.elems . stagePoints) graph
    where
    nodesBefore :: (DynGraph gr) => Node -> gr a b -> Set Node
    nodesBefore node graph = reachable' node $ grev graph
    nodesBeforeMany :: (DynGraph gr) => Set Node -> gr a b -> Set Node
    nodesBeforeMany nodes graph = join $ (`nodesBefore` graph) <$> nodes
    graphBefore :: (DynGraph gr) => Set Node -> gr a b -> gr a b
    graphBefore nodes graph = subgraph' (nodesBeforeMany nodes graph) graph

stagePoints :: (DynGraph gr) => gr a b -> Set (Set Node)
stagePoints graph = do
    x <- nodes' graph
    let i = inde x graph -- These are the independent events in the graph.
    y <- stagePoints i `mappend` (return mempty) -- Any other,
                                            -- independent event may have happened before x,
                                            -- Or none at all!
    return (return x `mappend` y)

type Stage = Set Actor

action :: Stage -> Event -> Stage
action stage event@ (Enter actor)
    | actor `member` stage = error ("Cannot apply " ++ show event ++ "! " ++ show actor ++ " is already onstage!")
    | otherwise = actor `insert` stage
action stage event@ (Exit actor)
    | actor `member` stage = actor `delete` stage
    | otherwise = error ("Cannot apply " ++ show event ++ "! " ++ show actor ++ " is not onstage!")
action stage None = stage


firstNodes :: (DynGraph gr) => gr a b -> Set Node
firstNodes graph = (`firstNode` graph) <$> nodes' graph

firstNode node graph = head . reverse . (reachable node) $ (grev graph)

tree :: Timeline -> Tree Event
tree graph = Node None (fmap (fromJust . (lab graph)) <$> dff'' (firstNodes graph) graph)

fold :: Tree Event -> Stage
fold = foldl' action mempty

