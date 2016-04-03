module GCJ where

class Problem p where
  parse :: [String] -> [p]
  parseExamples :: [(String, [p])]

class Solution s where
  display :: Int -> s -> String
  displayExamples :: [([s], String)]

class Runner s where
  solve :: s -> String -> String

data R p s = R (p -> s)
instance (Problem p, Solution s) => GCJ.Runner (R p s) where
  solve (R r) = unlines . zipWith display [1..] . map r . parse . tail . lines
