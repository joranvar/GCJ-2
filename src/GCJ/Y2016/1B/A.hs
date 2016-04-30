{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limitsOf)
import qualified Test.QuickCheck as QS
import Data.Bits (bit, shift)
import Data.Digits (digits, unDigits)
import Data.List (unfoldr, find, sortOn)
import Data.List (group, break, inits, tails, intersperse, sort)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, isNothing, fromJust)
import Control.Arrow (first, second)

data P = P String
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P s : parse rest
    where s:rest = inp

  parseExamples = [ ( "4\nOZONETOWER\nWEIGHFOXTOURIST\nOURNEONFOE\nETHER\n"
                    , [ P "OZONETOWER", P "WEIGHFOXTOURIST", P "OURNEONFOE", P "ETHER" ] ) ]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate 20
                            , testRuntime = 4 * 60 * 1000 * 1000
                            , numCases = 100 }
                  , TestSet { name = "Large"
                            , generator = generate 2000
                            , testRuntime = 8 * 60 * 1000 * 1000
                            , numCases = 100 } ]
    where generate maxLen =  do
            (n, nLabel) <- GCJ.limitsOf 3 maxLen $ QS.elements ['A'..'Z']
            return ( P n
                   , map (uncurry (++) . second show) [("s:", nLabel)])

data S = S [Int]
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S xs) = "Case #" ++ show n ++ ": " ++ concatMap show xs
  displayExamples = [( [S [0,1,2], S [2,4,6,8], S [1,1,4], S [3]]
                     , "Case #1: 012\nCase #2: 2468\nCase #3: 114\nCase #4: 3\n" )]

data R = R
instance GCJ.Runner R P S where
  solve R (P ss) = S $ map length groups
    where groups = group . sort $ ss
          zs = length $ filter (=='Z') ss

  props R =
    [ ( "True"
      , \(P s) (S ss) -> True )
    ]
