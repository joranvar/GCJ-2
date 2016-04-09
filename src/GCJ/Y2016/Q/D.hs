{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limits)
import qualified Test.QuickCheck as QS

data Tile = Gold | Lead deriving (Eq, Show)

data P = P Int Int Int
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P k c s : parse rest
    where kcs:rest = inp
          k:c:s:_ = map read $ words kcs

  parseExamples = [ ( "5\n2 3 2\n1 1 1\n2 1 1\n2 1 2\n3 2 3\n"
                    , [ P 2 3 2, P 1 1 1, P 2 1 1, P 2 1 2, P 3 2 3] ) ]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate True `QS.suchThat` kcConstraint
                            , testRuntime = 4 * 60 * 1000 * 1000
                            , numCases = 100 }
                  , TestSet { name = "Large"
                            , generator = generate False `QS.suchThat` kcConstraint
                            , testRuntime = 8 * 60 * 1000 * 1000
                            , numCases = 100 } ]
    where
      kcConstraint ((P k c _), _) = (((fromIntegral k)::Integer)^((fromIntegral c)::Integer)) <= ((10::Integer)^(18::Integer))
      generate sIsK =  do
            (k, kLabel) <- GCJ.limits 1 100
            (c, cLabel) <- GCJ.limits 1 100
            (s, sLabel) <- GCJ.limits 1 100
            return ( P k c (if sIsK then k else s)
                   , concat [ map ("k:" ++ ) kLabel
                            , map ("c:" ++ ) cLabel
                            , map ("s:" ++ ) (if sIsK then kLabel else sLabel)])

data S = S [Int] | Impossible
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n Impossible = "Case #" ++ show n ++ ": IMPOSSIBLE"
  display n (S is) = "Case #" ++ show n ++ ":" ++ concatMap (\i -> " " ++ show i) is
  displayExamples = [( [S [2], S [1], Impossible, S [1, 2], S [2, 6]]
                     , "Case #1: 2\nCase #2: 1\nCase #3: IMPOSSIBLE\nCase #4: 1 2\nCase #5: 2 6\n" )]

data R = R
instance GCJ.Runner R P S where
  solve R (P k 1 s) | k == s = S [1..s]
  solve R _ = Impossible

  props R =
    [
    ]
