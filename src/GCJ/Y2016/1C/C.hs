{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limitsOf)
import qualified Test.QuickCheck as QS
import Control.Arrow (second)

data P = P
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P : parse rest
    where _:rest = inp

  parseExamples = [ ( "1\n\n"
                    , [ P ] ) ]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate 20
                            , testRuntime = 4 * 60 * 1000 * 1000
                            , numCases = 100 }
                  , TestSet { name = "Large"
                            , generator = generate 2000
                            , testRuntime = 8 * 60 * 1000 * 1000
                            , numCases = 100 } ]
    where generate maxLen =  do
            (_, nLabel) <- GCJ.limitsOf 3 maxLen $ QS.elements ['A'..'Z']
            return ( P
                   , map (uncurry (++) . second show) [("s:", nLabel)])

data S = S
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n S = "Case #" ++ show n ++ ": "
  displayExamples = [( [S]
                     , "Case #1: \n" )]

data R = R
instance GCJ.Runner R P S where
  solve R P = S

  props R =
    [ ( "True"
      , \P S -> True )
    ]
