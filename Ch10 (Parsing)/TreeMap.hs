module TreeMap where

import Prelude hiding (Functor (..))

data Tree a
  = Node (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just x) = Just (f x)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node a b) = Node (fmap f a) (fmap f b)

treeLengths (Leaf s) = Leaf (length s)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)
