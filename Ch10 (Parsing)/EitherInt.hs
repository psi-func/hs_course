{-# LANGUAGE FlexibleInstances #-}

-- {-# LANGUAGE OverlappingInstances #-}

module EitherInt where

-- import Data.Either hiding (Functor (..))

instance {-# OVERLAPPING #-} Functor (Either Int) where
  fmap _ (Left n) = Left n
  fmap f (Right r) = Right (f r)