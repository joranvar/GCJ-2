module GCJ where

class Problem p where
  parse :: [String] -> [p]
