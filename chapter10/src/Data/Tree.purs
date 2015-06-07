module Data.Tree where

import Data.JSON
import Data.Foreign
import Data.Foreign.Class
import Data.Maybe

data Tree a = Leaf a | Branch (Tree a) (Tree a)

-- Format: where "value0" is Just Number and "{}" is Nothing

-- {
--   "tag":"Branch",
--   "value":{},
--   "left":{
--     "value0":{
--       "tag":"Leaf",
--       "value":{
--         "value0":1
--       },
--       "left":{},
--       "right":{}
--     }
--   },
--   "right":{
--     "value0":{
--       "tag":"Leaf",
--       "value":{
--         "value0":2
--       },
--       "left":{},
--       "right":{}
--     }
--   }
-- }

newtype RTree a = RTree { tag :: String, value :: Maybe a, left :: Maybe (RTree a), right :: Maybe (RTree a) }

stringifyTree :: forall a. Tree a -> String
stringifyTree = stringify <<< toForeign <<< f
  where
  f (Leaf x)     = RTree { tag: "Leaf",   value: Just x,  left: Nothing,    right: Nothing}
  f (Branch l r) = RTree { tag: "Branch", value: Nothing, left: Just (f l), right: Just (f r) }

instance treeIsForeign :: (IsForeign a) => IsForeign (Tree a) where
  read root = readProp "tag" root >>= \tag -> case tag of
    "Leaf" -> readProp "value" root
      >>= \maybeValue -> readProp "value0" maybeValue
      >>= \value -> return $ Leaf value
    "Branch" -> readProp "left" root
      >>= \maybeLeft -> readProp "value0" maybeLeft
      >>= \left -> read left
      >>= \left' -> readProp "right" root
      >>= \maybeRight -> readProp "value0" maybeRight
      >>= \right -> read right
      >>= \right' -> return $ Branch left' right'

instance showTree :: (Show a) => Show (Tree a) where
show (Leaf x) = "Leaf " ++ show x
show (Branch left right) = "Branch (" ++ show left ++ ") (" ++ show right ++ ")"
