{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import GHC.Generics
import Lib

data Values = Values
  { a :: Int
  , b :: String
  } deriving (Generic, Show)

data Functions = Functions
  { a :: Int -> Int
  , b :: String -> String
  } deriving (Generic)

values :: Values
values = Values
  { a = 1
  , b = "pen"
  }

functions :: Functions
functions = Functions
  { a = (+) 1
  , b = (++) "apple"
  }

main :: IO ()
main = do
  print $ pairwiseApply functions values
  -- output:
  -- Values {a = 2, b = "applepen"}
