
module Data.Graph.Inductive.Set where

import Data.Set.Monad hiding (union, intersection, difference, (\\))
import qualified Data.Set.Monad as S
import Data.Graph.Inductive hiding (reachable, nodes, subgraph, dff)
import qualified Data.Graph.Inductive as G

type Nodes = Set Node

reachable :: DynGraph gr => Node -> gr a b -> Nodes
reachable = (fromList .) . G.reachable

extent :: DynGraph gr => gr a b -> Node -> Nodes
extent = flip reachable

-- reachable' :: (DynGraph gr) => GDecomp gr a b -> Nodes
-- reachable' ((_,node,_,_), graph) = reachable node graph

nodes :: Graph gr => gr a b -> Nodes
nodes = fromList . G.nodes

subgraph :: DynGraph gr => Nodes -> gr a b -> gr a b
subgraph = G.subgraph . elems

isolate :: DynGraph gr => gr a b -> Nodes -> gr a b
isolate = flip subgraph

dff nodes = G.dff (elems nodes)

difference :: DynGraph gr => gr a b -> gr a b -> gr a b
difference x y = subgraph (nodes x S.\\ nodes y) x

infix 5 \\
(\\) :: DynGraph gr => gr a b -> gr a b -> gr a b
(\\) = difference

union :: DynGraph gr => gr a b -> gr a b -> gr a b
union x y = labEdges y `insEdges` (labNodes y `insNodes` x)

intersection :: DynGraph gr => gr a b -> gr a b -> gr a b
intersection x y = isolate x (nodes x `S.union` nodes y)
