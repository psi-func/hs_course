module PNM where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

data Greymap = Greymap
  { greyWidth :: Int,
    greyHeght :: Int,
    greyMax :: Int,
    greyData :: L.ByteString
  }
  deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m
