module GCJ where

import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QS

data TestSet p = TestSet { name::String, generator::Gen p, testRuntime::Int, numCases::Int }

class Problem p where
  parse :: [String] -> [p]
  parseExamples :: [(String, [p])]
  setGenerators :: [TestSet (p, [String])]

class Solution s where
  display :: Int -> s -> String
  displayExamples :: [([s], String)]

class Runner s where
  solve :: s -> String -> String

data R p s = R (p -> s)
instance (Problem p, Solution s) => GCJ.Runner (R p s) where
  solve (R r) = unlines . zipWith display [1..] . map r . parse . tail . lines

limits :: (QS.Arbitrary a) => Int -> Int -> Gen ([a], [String])
limits min' max' = do
  n  <- QS.oneof [QS.choose (min', max'), QS.elements [min', max']]
  xs <- QS.vector n
  return ( xs, case n of _ | n == min' -> ["min"]
                         _ | n == max' -> ["max"]
                         _             -> [] )

limitsOf :: Int -> Int -> [a] -> Gen ([a], [String])
limitsOf min' max' xs = do
  n  <- QS.oneof [QS.choose (min', max'), QS.elements [min', max']]
  ys <- QS.vectorOf n $ QS.elements xs
  return ( ys, case n of _ | n == min' -> ["min"]
                         _ | n == max' -> ["max"]
                         _             -> [] )
