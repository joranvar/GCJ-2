{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module GCJ where

import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QS
import System.Random (Random)

data TestSet p = TestSet { name::String, generator::Gen p, testRuntime::Int, numCases::Int }

class Problem p where
  parse :: [String] -> [p]
  parseExamples :: [(String, [p])]
  setGenerators :: [TestSet (p, [String])]

class Solution s where
  display :: Int -> s -> String
  displayExamples :: [([s], String)]

class (Problem p, Solution s) => Runner r p s | r -> p s where
  interactor :: r -> String -> String
  solve :: r -> p -> s
  props :: r -> [(String, p -> s -> Bool)]
  tests :: r -> [(String, p, s -> Bool)]
  interactor r = unlines . zipWith display [1..] . map (solve r) . parse . tail . lines
  props _ = []
  tests _ = []

limits :: (Random a, Eq a) => a -> a -> Gen (a, [String])
limits min' max' = do
  n  <- QS.oneof [QS.choose (min', max'), QS.elements [min', max']]
  return ( n, case n of _ | n == min' -> ["min"]
                        _ | n == max' -> ["max"]
                        _             -> [] )

limitsOf :: Int -> Int -> Gen a -> Gen ([a], [String])
limitsOf min' max' xs = do
  n  <- QS.oneof [QS.choose (min', max'), QS.elements [min', max']]
  ys <- QS.vectorOf n xs
  return ( ys, case n of _ | n == min' -> ["min"]
                         _ | n == max' -> ["max"]
                         _             -> [] )
