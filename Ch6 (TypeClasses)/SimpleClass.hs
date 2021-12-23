{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module SimpleClass where

import Data.List

class Foo a where
  foo :: a -> String

instance (Foo a) => Foo [a] where
  foo = intercalate ", " . map foo

instance Foo Char where
  foo c = [c]

instance Foo String where
  foo = id
