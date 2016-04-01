module GCJ where

class Problem p where
  parse :: [String] -> [p]
  parseExamples :: [(String, [p])]

class Solution s where
  display :: s -> String
  displayExamples :: [([s], String)]
