# records-fields-pairwise

a demo with GHC8 Generics and Records making use of stuff defined in generic-lens, with a type family originally implemented by [kcsongor](https://github.com/kcsongor) for converting GHC8 Generic Record data types to a list of tuples of symbol and type, similar to RowLists in PureScript.

probably would only be useful in libraries, or in a speicific domain like doing something like my pairwise routes to handlers thing in my [vidtracker](https://github.com/justinwoo/vidtracker) project.

## Tl;dr

```hs
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
```
