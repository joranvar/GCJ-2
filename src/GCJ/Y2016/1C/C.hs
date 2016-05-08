{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limits)
import qualified Test.QuickCheck as QS
import Control.Arrow (second)

data P = P Int Int Int Int
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P j p s k : parse rest
    where jpsk:rest = inp
          [j,p,s,k] = map read $ words jpsk

  parseExamples = [ ( "4\n1 1 1 10\n1 2 3 2\n1 1 3 2\n1 2 3 1\n"
                    , [ P 1 1 1 10, P 1 2 3 2, P 1 1 3 2, P 1 2 3 1 ] ) ]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate 20
                            , testRuntime = 4 * 60 * 1000 * 1000
                            , numCases = 100 }
                  , TestSet { name = "Large"
                            , generator = generate 2000
                            , testRuntime = 8 * 60 * 1000 * 1000
                            , numCases = 100 } ]
    where generate maxLen =  do
            (j, jLabel) <- GCJ.limits 3 maxLen
            (p, pLabel) <- GCJ.limits 3 maxLen
            (s, sLabel) <- GCJ.limits 3 maxLen
            (k, kLabel) <- GCJ.limits 3 maxLen
            return ( P j p s k
                   , map (uncurry (++) . second show) [("j:", jLabel),("p:", pLabel),("s:", sLabel),("k:", kLabel)])

data S = S
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n S = "Case #" ++ show n ++ ": "
  displayExamples = [( [S]
                     , "Case #1: \n" )]

data R = R
instance GCJ.Runner R P S where
  solve R (P j p s k) = S

  props R =
    [ ( "True"
      , \(P j p s k) S -> True )
    ]
