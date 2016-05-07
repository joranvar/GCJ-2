{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limitsOf)
import qualified Test.QuickCheck as QS
import Data.List (group, sort)
import Control.Arrow (second)

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
  solve R (P ss) = S $ zeros ++ ones ++ twos ++ threes ++ fours ++ fives ++ sixes ++ sevens ++ eights ++ nines
    where zeros = map (const 0) $ filter (=='Z') ss
          twos = map (const 2) $ filter (=='W') ss
          fours = map (const 4) $ filter (=='U') ss
          sixes = map (const 6) $ filter (=='X') ss
          fives = flip replicate 5 $ (length $ filter (=='F') ss) - length fours
          sevens = flip replicate 7 $ (length $ filter (=='V') ss) - length fives
          eights = map (const 8) $ filter (=='G') ss
          ones = flip replicate 1 $ (length $ filter (=='O') ss) - length (fours++twos++zeros)
          threes = flip replicate 3 $ (length $ filter (=='H') ss) - length eights
          nines = flip replicate 9 $ (length $ filter (=='I') ss) - length (fives++sixes++eights)

  props R =
    [ ( "True"
      , \(P s) (S ss) -> True )
    ]
