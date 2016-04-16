{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limitsOf)
import qualified Test.QuickCheck as QS
import Data.Bits (bit, shift)
import Data.Digits (digits, unDigits)
import Data.List (unfoldr, find, sortOn)
import Data.List (group)
import Data.Maybe (catMaybes, isNothing, fromJust)
import Control.Arrow (second)

data P = P [Char]
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P s : parse rest
    where s:rest = inp

  parseExamples = [ ( "7\nCAB\nJAM\nCODE\nABAAB\nCABCBBABC\nABCABCABC\nZXCASDQWE\n"
                    , [ P "CAB", P "JAM", P "CODE", P "ABAAB", P "CABCBBABC", P "ABCABCABC", P "ZXCASDQWE" ] ) ]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate 15
                            , testRuntime = 4 * 60 * 1000
                            , numCases = 100 }
                  , TestSet { name = "Large"
                            , generator = generate 1000
                            , testRuntime = 8 * 60 * 1000
                            , numCases = 100 } ]
    where generate maxLen =  do
            (n, nLabel) <- GCJ.limitsOf 1 maxLen $ QS.elements ['A'..'Z']
            return ( P n
                   , concat [ map ("s:" ++ ) nLabel ])

data S = S String
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S s) = "Case #" ++ show n ++ ": " ++ s
  displayExamples = [( [S "CAB", S "MJA", S "OCDE", S "BBAAA", S "CCCABBBAB", S "CCCBAABAB", S "ZXCASDQWE" ]
                     , "Case #1: CAB\nCase #2: MJA\nCase #3: OCDE\nCase #4: BBAAA\nCase #5: CCCABBBAB\nCase #6: CCCBAABAB\nCase #7: ZXCASDQWE\n" )]

data R = R
instance GCJ.Runner R P S where
  solve R (P ss) = S ""

  props R =
    [ ( "Some prop"
      , \p s -> True )
    ]
