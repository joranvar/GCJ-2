{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import Data.List (delete)
import qualified Data.Set as Set
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limits, limitsOf)
import qualified Test.QuickCheck as QS

newtype SearchEngine = SearchEngine String deriving (Eq, Show, Ord)
newtype Query        = Query String        deriving (Eq, Show)

data P = P (Set.Set SearchEngine) [Query]
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P (Set.fromList $ map SearchEngine ss) (map Query qs) : parse rest
    where s:spart     = inp
          (ss, qpart) = splitAt (read s) spart
          q:qpart'    = qpart
          (qs, rest)  = splitAt (read q) qpart'

  parseExamples = [ ( "2\n5\nYeehaw\nNSM\nDont Ask\nB9\nGoogol\n10\nYeehaw\nYeehaw\nGoogol\nB9\nGoogol\nNSM\nB9\nNSM\nDont Ask\nGoogol\n5\nYeehaw\nNSM\nDont Ask\nB9\nGoogol\n7\nGoogol\nDont Ask\nNSM\nNSM\nYeehaw\nYeehaw\nGoogol\n"
                    , [  P (Set.fromList $ map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                           (map Query ["Yeehaw","Yeehaw","Googol","B9","Googol","NSM","B9","NSM","Dont Ask","Googol"])
                      ,  P (Set.fromList $ map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                           (map Query ["Googol","Dont Ask","NSM","NSM","Yeehaw","Yeehaw","Googol"])])]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate (2, 10) (0, 100)
                            , testRuntime = 4 * 60 * 1000
                            , numCases = 20 }
                  , TestSet { name = "Large"
                            , generator = generate (2, 100) (0, 1000)
                            , testRuntime = 8 * 60 * 1000
                            , numCases = 20 } ]
    where generate (minS, maxS) (minQ, maxQ) =  do
            (ss, sLabel) <- GCJ.limits minS maxS
            (qs, qLabel) <- GCJ.limits minQ maxQ
            (overlappers, oLabel) <- GCJ.limitsOf 0 (length qs) ss
            qs' <- QS.shuffle $ take (length qs) (overlappers ++ qs)
            return ( P (Set.fromList $ map SearchEngine ss) (map Query qs')
                   , concat [ map ("s:" ++ ) sLabel
                            , map ("q:" ++) qLabel
                            , map ("overlap:" ++) oLabel ])

data S = S Int
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S i) = "Case #" ++ show n ++ ": " ++ show i
  displayExamples = [([S 1, S 0], "Case #1: 1\nCase #2: 0\n")]

data R = R
instance GCJ.Runner R P S where
  solve R (P _ []) = S 0
  solve R (P ss qs) = S $ result 0 (Set.elems ss) qs
    where result acc _ [] = acc
          result acc [] qs' = result (acc + 1) (Set.elems ss) qs'
          result acc ss' (Query q:qs') = result acc (delete (SearchEngine q) ss') qs'
