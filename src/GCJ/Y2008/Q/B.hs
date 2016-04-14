{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limitsOf, limits)
import qualified Test.QuickCheck as QS
import Data.List (sortOn)
import Data.Ord (comparing)

data Time = Time { hh::Int, mm::Int }
          deriving (Eq, Show)
instance Ord Time where
  compare = comparing minsOfTime

timeOfMins :: Int -> Time
timeOfMins mins = Time (mins `div` 60) (mins `mod` 60)

minsOfTime :: Time -> Int
minsOfTime time = hh time * 60 + mm time

data TripInformation = TripInformation { depart::Time
                                       , arrive::Time }
                     deriving (Eq, Show)

data P = P { turnaroundTime::Int
           , tripsAB::[TripInformation]
           , tripsBA::[TripInformation] }
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P (read t) ab ba : parse rest
    where t:trips = inp
          nanb:abpart = trips
          na:nb:_ = map read $ words nanb
          (ab', bapart) = splitAt na abpart
          (ba', rest) = splitAt nb bapart
          ab = map parseTimes ab'
          ba = map parseTimes ba'
          parseTimes s =
            let h:m:h':m':_ = map read . words . map (\c -> case c of
                                                         ':' -> ' '
                                                         other -> other) $ s
            in TripInformation (Time h m) (Time h' m')

  parseExamples = [ ( "2\n5\n3 2\n09:00 12:00\n10:00 13:00\n11:00 12:30\n12:02 15:00\n09:00 10:30\n2\n2 0\n09:00 09:01\n12:00 12:02\n"
                    , [ P 5 [ TripInformation (Time 9 0) (Time 12 0)
                            , TripInformation (Time 10 0) (Time 13 0)
                            , TripInformation (Time 11 0) (Time 12 30)
                            ]
                        [ TripInformation (Time 12 2) (Time 15 0)
                        , TripInformation (Time 9 0) (Time 10 30)
                        ]
                      , P 2 [ TripInformation (Time 9 0) (Time 9 1)
                            , TripInformation (Time 12 0) (Time 12 2)
                            ]
                        [ ]
                      ] ) ]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate (0, 20) (0, 5)
                            , testRuntime = 4 * 60 * 1000 * 1000
                            , numCases = 100 }
                  , TestSet { name = "Large"
                            , generator = generate (0, 100) (0, 60)
                            , testRuntime = 8 * 60 * 1000 * 1000
                            , numCases = 100 } ]
    where generate (minN, maxN) (minT, maxT) =  do
            (na, aLabel) <- GCJ.limitsOf minN maxN generateTripInformation
            (nb, bLabel) <- GCJ.limitsOf minN maxN generateTripInformation
            (t, tLabel) <- GCJ.limits minT maxT
            return ( P t na nb
                   , concat [ map ("na:" ++ ) aLabel
                            , map ("nb:" ++) bLabel
                            , map ("t:" ++) tLabel ])
          generateTripInformation = do
            departTime <- QS.choose (0, (24 * 60) - 2)
            tripTime <- QS.choose (1, (24 * 60) - departTime)
            return $ TripInformation (timeOfMins departTime) (timeOfMins $ departTime + tripTime)

data S = S Int Int
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S a b) = "Case #" ++ show n ++ ": " ++ show a ++ " " ++ show b
  displayExamples = [([S 2 2, S 2 0], "Case #1: 2 2\nCase #2: 2 0\n")]

data R = R
instance GCJ.Runner R P S where
  solve R (P t as bs) =
    S (solve' (sortOn depart as) (sortOn arrive bs)) (solve' (sortOn depart bs) (sortOn arrive as))
    where solve' [] _ = 0
          solve' (x:xs) (y:ys) | (minsOfTime $ depart x) >= (t + (minsOfTime $ arrive y)) = solve' xs ys
          solve' (_:xs) ys = 1 + solve' xs ys

  props R =
    [ ( "At least"
      , \(P _ as bs) (S a b) -> a >= length as - length bs && b >= length bs - length as
      )
    ]
