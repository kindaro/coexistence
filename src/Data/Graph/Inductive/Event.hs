module Data.Graph.Inductive.Event where

import Data.Set.Monad (Set, elems, fromList)
import Data.Tree (Tree(..))
import Data.Maybe (maybeToList)
import Data.Graph.Inductive hiding (nodes)
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.Set
import Data.Graph.Inductive.TotalOrder

type Fabric event = Gr event () -- ^ One fabric of events.

type Moment = Nodes -- ^ A section that cuts the fabric in two parts.

-- TODO: If `chronology` works, I can remove this.
-- past :: Fabric -> [Fabric] -- ^ Unweaving the fabric.
-- past graph = (`graphBefore` graph) <$> (Set.elems . moments) graph
--     where
--     nodesBefore :: (DynGraph gr) => Node -> gr a b -> Set Node
--     nodesBefore node graph = reachable' node $ grev graph
--     nodesBeforeMany :: (DynGraph gr) => Set Node -> gr a b -> Set Node
--     nodesBeforeMany nodes graph = join $ (`nodesBefore` graph) <$> nodes
--     graphBefore :: (DynGraph gr) => Set Node -> gr a b -> gr a b
--     graphBefore nodes graph = subgraph' (nodesBeforeMany nodes graph) graph

chronology :: Fabric event -> [Fabric event]
chronology graph = past graph <$> elems (moments graph)

future :: Fabric event -> Moment -> Fabric event
future graph moment = isolate graph . fromList . mconcat $ elems . extent graph <$> elems moment

past :: Fabric event -> Moment -> Fabric event
past = (grev .) . future . grev

moments :: Fabric event -> Set Moment
moments graph = do -- TODO: doesn't work with Set.
    x <- nodes graph
    let i = inde x graph -- These are the independent events in the graph.
    y <- moments i `mappend` return mempty  -- Any independent event may have
                                            -- happened before x,
                                            -- or none at all!
    return (return x `mappend` y)

-- firstNodes :: (DynGraph gr) => gr a b -> Set Node
-- firstNodes graph = (`firstNode` graph) <$> nodes' graph
-- 
-- firstNode node graph = head . reverse . (reachable node) $ (grev graph)
-- 
-- tree :: Fabric -> Tree Event
-- tree graph = Node None (fmap (fromJust . (lab graph)) <$> dff'' (firstNodes graph) graph)

tree :: Fabric event -> Tree [event]
tree graph = Node mempty $ fmap (maybeToList . lab graph) <$> dff' graph
