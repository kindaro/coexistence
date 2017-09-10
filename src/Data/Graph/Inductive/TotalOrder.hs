module Data.Graph.Inductive.TotalOrder where

import Data.Graph.Inductive hiding (reachable, nodes, subgraph, dff)
import Data.Graph.Inductive.Set
-- import Data.Graph.Inductive.Canon

-- newtype Chain gr = Chain { fromChain :: gr }
-- 
-- toChain :: DynGraph gr => gr a b -> Maybe (Chain (gr a b))
-- toChain graph
--     | isEmpty graph = Just $ Chain graph
--     | tord graph `iso` graph = Just $ Chain graph -- TODO: isol -- stronger than isomorphism:
--                                                     -- account for labels.
--     | otherwise = Nothing

tord :: DynGraph gr => Node -> gr a b -> gr a b -- ^ Totally ordered subgraph at given node.
-- TODO: Flip.
--
-- TODO: This is not in fact a total order: some of the nodes other than the given one can be out
-- of order inbetween themselves.
--
-- TODO: Rename. It's not total order. Just -- should I say -- dependent?
tord node graph = subgraph nodes graph
    where
    nodes :: Nodes
    -- TODO: The same result can be achieved with extent (undir graph) node
    -- HAHA no. It can't. Nothing stops it from spilling away from the nodes that are reachable by
    -- following only one direction (either along or against the edges) to the nodes reachable by
    -- following the edges in mixed directions, thus being the same as the whole connected subgraph.
    nodes = reachable node graph `mappend` reachable node (grev graph)
    -- TODO: oneliner?

ordt :: DynGraph gr => gr a b -> Node -> gr a b
ordt = flip tord

inde :: DynGraph gr => Node -> gr a b -> gr a b -- ^ Subgraph independent from given node.
-- TODO: Flip.
inde node graph = graph \\ tord node graph

first :: DynGraph gr => gr a b -> Node -> Node -- ^ First node in chain.
first graph = head . topsort . ordt graph
-- last :: DynGraph gr => gr a b -> Node -> Node
