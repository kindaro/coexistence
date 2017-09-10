module Data.Stage where

import Data.Set.Monad
import Data.Tree (Tree(..))

data Actor = Us | Cat | Nina | Pasha | Tenant
    deriving (Eq, Ord, Show) -- TODO: Remove Ord?

data Event = Enter Actor | Exit Actor | None
    deriving (Eq, Ord, Show)

type Stage = Set Actor

action :: Stage -> Event -> Stage
action stage event@ (Enter actor)
    | actor `member` stage = error ("Cannot apply " ++ show event ++ "! " ++ show actor ++ " is already onstage!")
    | otherwise = actor `insert` stage
action stage event@ (Exit actor)
    | actor `member` stage = actor `delete` stage
    | otherwise = error ("Cannot apply " ++ show event ++ "! " ++ show actor ++ " is not onstage!")
action stage None = stage


fold :: Tree Event -> Stage
fold = Prelude.foldl action mempty
