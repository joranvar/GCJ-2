{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limitsOf)
import qualified Test.QuickCheck as QS

data Pancake = Smile | Blank deriving (Eq, Show)

data P = P [Pancake]
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P (map parsePancake n) : parse rest
    where n:rest = inp
          parsePancake '-' = Blank
          parsePancake '+' = Smile
          parsePancake _ = undefined

  parseExamples = [ ( "5\n-\n-+\n+-\n+++\n--+-\n"
                    , [ P [Blank], P [Blank, Smile], P [Smile, Blank], P [Smile, Smile, Smile]
                      , P [Blank, Blank, Smile, Blank] ] ) ]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate (1, 10)
                            , testRuntime = 4 * 60 * 1000
                            , numCases = 100 }
                  , TestSet { name = "Large"
                            , generator = generate (1, 100)
                            , testRuntime = 8 * 60 * 1000
                            , numCases = 100 } ]
    where generate (minN, maxN) =  do
            (n, nLabel) <- GCJ.limitsOf minN maxN $ QS.elements [Blank, Smile]
            return ( P n
                   , concat [ map ("s:" ++ ) nLabel ])

data S = S Int
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S i) = "Case #" ++ show n ++ ": " ++ show i
  displayExamples = [( [S 1, S 1, S 2, S 0, S 3]
                     , "Case #1: 1\nCase #2: 1\nCase #3: 2\nCase #4: 0\nCase #5: 3\n" )]

data R = R
instance GCJ.Runner R P S where
  solve R (P _) = S 0

  props R =
    [ 
    ]
