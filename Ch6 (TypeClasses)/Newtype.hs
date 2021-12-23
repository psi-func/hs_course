module Newtype where

data DataInt = D Int
  deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int
  deriving (Eq, Ord, Show)

instance Num NewtypeInt where
  (N a) + (N b) = N (a + b)

  (N a) * (N b) = N (a * b)

  abs (N a) = N (abs a)

  signum (N a) = N (signum a)

  fromInteger a = N (fromInteger a)

  negate (N a) = N (negate a)

newtype UniqueID = UniqueID Int
  deriving (Eq)
