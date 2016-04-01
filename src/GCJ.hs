module GCJ where

class Problem p where
  parse :: [String] -> [p]
  parseExamples :: [(String, [p])]

class Solution s where
  display :: Int -> s -> String
  displayExamples :: [([s], String)]

class Solver s where
  solve :: s -> [String] -> [String]

data R p s = R (p -> s)
instance (Problem p, Solution s) => GCJ.Solver (R p s) where
  solve (R r) = zipWith display [1..] . map r . parse
