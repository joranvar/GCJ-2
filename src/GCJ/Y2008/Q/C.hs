{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limits)

data P = P Float Float Float Float Float
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P f r t r' g : parse rest
    where frtrg:rest = inp
          f:r:t:r':g:_ = map read $ words frtrg

  parseExamples = [ ( "5\n0.25 1.0 0.1 0.01 0.5\n0.25 1.0 0.1 0.01 0.9\n0.00001 10000 0.00001 0.00001 1000\n0.4 10000 0.00001 0.00001 700\n1 100 1 1 10\n"
                    , [ P 0.25 1.0 0.1 0.01 0.5
                      , P 0.25 1.0 0.1 0.01 0.9
                      , P 0.00001 10000.0 0.00001 0.00001 1000.0
                      , P 0.4 10000.0 0.00001 0.00001 700.0
                      , P 1.0 100.0 1.0 1.0 10.0
                      ] ) ]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate
                            , testRuntime = 4 * 60 * 1000 * 1000
                            , numCases = 30 }
                  , TestSet { name = "Large"
                            , generator = generate
                            , testRuntime = 8 * 60 * 1000 * 1000
                            , numCases = 100 } ]
    where generate = do
            (r, rLabel) <- GCJ.limits 0.0 10000.0
            (f, fLabel) <- GCJ.limits 0.0 r
            (t, tLabel) <- GCJ.limits 0.0 r
            (r', r'Label) <- GCJ.limits 0.0 r
            (g, gLabel) <- GCJ.limits 0.0 10000.0
            return ( P f r t r' g
                   , concat [ map ("R:" ++ ) rLabel
                            , map ("f:" ++ ) fLabel
                            , map ("t:" ++ ) tLabel
                            , map ("r:" ++ ) r'Label
                            , map ("g:" ++ ) gLabel ])

data S = S Float
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S s) = "Case #" ++ show n ++ ": " ++ show s
  displayExamples = [([S 1.0, S 0.910015, S 0.0, S 0.002371, S 0.573972], "Case #1: 1.000000\nCase #2: 0.910015\nCase #3: 0.000000\nCase #4: 0.002371\nCase #5: 0.573972\n")]

data R = R
instance GCJ.Runner R P S where
  solve R (P _ _ _ _ _) = S 0.0
