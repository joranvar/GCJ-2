{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limits)

data P = P Int
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P (read n) : parse rest
    where n:rest = inp

  parseExamples = [ ( "5\n0\n1\n2\n11\n1692\n"
                    , [  P 0, P 1, P 2, P 11, P 1692 ])]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate (0, 200)
                            , testRuntime = 4 * 60 * 1000
                            , numCases = 100 }
                  , TestSet { name = "Large"
                            , generator = generate (0, 10 ^ (6 :: Int))
                            , testRuntime = 8 * 60 * 1000
                            , numCases = 100 } ]
    where generate (minN, maxN) =  do
            (n, nLabel) <- GCJ.limits minN maxN
            return ( P n
                   , concat [ map ("s:" ++ ) nLabel ])

data S = S Int | Insomnia
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S i) = "Case #" ++ show n ++ ": " ++ show i
  display n Insomnia = "Case #" ++ show n ++ ": INSOMNIA"
  displayExamples = [( [Insomnia, S 10, S 90, S 110, S 5076]
                     , "Case #1: INSOMNIA\nCase #2: 10\nCase #3: 90\nCase #4: 110\nCase #5: 5076\n" )]

data R = R
instance GCJ.Runner R P S where
  solve R (P _ ) = S 0

  props R =
    [ 
    ]
