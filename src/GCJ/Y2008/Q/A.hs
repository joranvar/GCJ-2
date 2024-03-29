{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import Data.List (nub)
import qualified Data.Set as Set
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limitsOf)
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
                            , testRuntime = 4 * 60 * 1000 * 1000
                            , numCases = 20 }
                  , TestSet { name = "Large"
                            , generator = generate (2, 100) (0, 1000)
                            , testRuntime = 8 * 60 * 1000 * 1000
                            , numCases = 20 } ]
    where generate (minS, maxS) (minQ, maxQ) =  do
            (ss, sLabel) <- GCJ.limitsOf minS maxS QS.arbitrary `QS.suchThat` ((>minS) . length . nub . fst)
            (qs, qLabel) <- GCJ.limitsOf minQ maxQ QS.arbitrary
            (overlappers, oLabel) <- GCJ.limitsOf 0 (length qs) $ QS.elements ss
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
  solve R (P ss qs) = S $ result 0 ss qs
    where result acc _ [] = acc
          result acc ss' (Query q:qs') | ss' == Set.singleton (SearchEngine q) =
                                         result (acc + 1) (ss Set.\\ ss') qs'
          result acc ss' (Query q:qs') = result acc (Set.delete (SearchEngine q) ss') qs'

  props R =
    [ ( "Length"
      , \(P _ qs) (S i) -> i < length qs || i == 0 )
    , ( "Remove the vulgar"
      , \(P ss qs) (S i) ->
        let ss' = map (\(SearchEngine s) -> s) (Set.elems ss)
            qs' = map (\(Query q) -> q) qs
        in i < length (filter (`elem` ss') qs') || i == 0 )
    ]

  tests R =
    [ ( "One query, no switch"
      , P (Set.fromList $ map SearchEngine ["A", "B"]) ([Query "A"])
      , \(S s) -> s == 0 )
    , ( "Two queries, one switch"
      , P (Set.fromList $ map SearchEngine ["A", "B"]) (map Query ["A", "B"])
      , \(S s) -> s == 1 )
    , ( "Three queries, one switch"
      , P (Set.fromList $ map SearchEngine ["A", "B"]) (map Query ["A", "A", "B"])
      , \(S s) -> s == 1 )
    , ( "Three queries, two switches"
      , P (Set.fromList $ map SearchEngine ["A", "B"]) (map Query ["A", "A", "B", "A"])
      , \(S s) -> s == 2 )
    ]
