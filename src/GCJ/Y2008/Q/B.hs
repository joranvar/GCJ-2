{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limitsOf, limits)
import qualified Test.QuickCheck as QS

data Time = Time { hh::Int, mm::Int }
          deriving (Eq, Show)

timeOfMins :: Int -> Time
timeOfMins mins = Time (mins `div` 60) (mins `mod` 60)

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
          ab = []
          ba = []

  parseExamples = [ ( ""
                    , [] ) ]

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
            depart <- QS.choose (0, (24 * 60) - 2)
            tripTime <- QS.choose (1, (24 * 60) - depart)
            return $ TripInformation (timeOfMins depart) (timeOfMins $ depart + tripTime)

data S = S Int Int
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S a b) = "Case #" ++ show n ++ ": " ++ show a ++ " " ++ show b
  displayExamples = [([], "")]

data R = R
instance GCJ.Runner R P S where
  solve R (P t as bs) = S 0 0
