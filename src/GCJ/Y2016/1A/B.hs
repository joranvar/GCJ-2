{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limitsOf, limits)
import qualified Test.QuickCheck as QS
import Data.Bits (bit, shift)
import Data.Digits (digits, unDigits)
import Data.List (unfoldr, find, sortOn)
import Data.List (group, nub)
import Data.Maybe (catMaybes, isNothing, fromJust)
import Control.Arrow (second)

data P = P [[Int]]
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P (map (map read . words) ns) : parse rest
    where n:npart = inp
          (ns, rest) = splitAt (2 * (read n) - 1) npart

  parseExamples = [ ( "1\n3\n1 2 3\n2 3 5\n3 5 6\n2 3 4\n1 2 3\n"
                    , [ P [[1,2,3],[2,3,5],[3,5,6],[2,3,4],[1,2,3]]] ) ]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate 10
                            , testRuntime = 4 * 60 * 1000
                            , numCases = 50 }
                  , TestSet { name = "Large"
                            , generator = generate 50
                            , testRuntime = 8 * 60 * 1000
                            , numCases = 50 } ]
    where generate maxN =  do
            (n, nLabel) <- GCJ.limits 2 maxN
--            ns <- QS.vectorOf (2*n-1) $ QS.vectorOf n $ QS.choose (1, 2500) `QS.suchThat` (\ns' -> (length . nub $ ns') == length ns')
            return ( P [[]]
                   , concat [ map ("n:" ++ ) nLabel ])

data S = S [Int]
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S xs) = "Case #" ++ show n ++ ":" ++ concatMap (\x -> " " ++ show x) xs
  displayExamples = [( [S [3,4,6]]
                     , "Case #1: 3 4 6\n" )]

data R = R
instance GCJ.Runner R P S where
  solve R (P ns) = S []

  props R =
    [ ( "Some prop"
      , \p s -> True )
    ]
