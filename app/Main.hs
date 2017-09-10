module Main where

import Data.Graph.Inductive
import Data.Graph.Inductive.TotalOrder
import Data.Graph.Inductive.Set hiding (reachable)
import Data.Graph.Inductive.Event
import qualified Data.Set as Set
import qualified Data.Set.Monad as SetM
import Data.Stage
import Data.Tree
import Data.List (nub, zipWith4)
import Data.Maybe

example :: Fabric Event
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

stagePoints = moments

stages :: Fabric event -> [Fabric event]
stages g = past g <$> ms
    where ms = SetM.elems . moments $ g

main :: IO ()
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
    let trees = (fmap (fromMaybe None . listToMaybe) . tree) <$> graphs
    let stages = fold <$> trees
    let prettyGraphs = prettify <$> graphs
    let prettyTrees = drawTree . fmap show <$> trees
    let zipped = zipWith4
            (\g t s p -> print g >> putStrLn t >> print s >> putStrLn p >> putStrLn "")
                graphs prettyTrees stages prettyGraphs
    sequence zipped
    print (fmap SetM.elems $ nub stages)

