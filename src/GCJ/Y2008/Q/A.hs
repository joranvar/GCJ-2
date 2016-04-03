module Solution (P(..), S(..), solve') where
import Data.List (delete)
import GCJ (Problem(..), Solution(..))
import qualified Test.QuickCheck as QS

newtype SearchEngine = SearchEngine String deriving (Eq, Show)
newtype Query        = Query String        deriving (Eq, Show)

data P = P [SearchEngine] [Query]
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P (map SearchEngine ss) (map Query qs) : parse rest
    where s:spart     = inp
          (ss, qpart) = splitAt (read s) spart
          q:qpart'    = qpart
          (qs, rest)  = splitAt (read q) qpart'

  parseExamples = [ ( "2\n5\nYeehaw\nNSM\nDont Ask\nB9\nGoogol\n10\nYeehaw\nYeehaw\nGoogol\nB9\nGoogol\nNSM\nB9\nNSM\nDont Ask\nGoogol\n5\nYeehaw\nNSM\nDont Ask\nB9\nGoogol\n7\nGoogol\nDont Ask\nNSM\nNSM\nYeehaw\nYeehaw\nGoogol\n"
                    , [  P (map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                           (map Query ["Yeehaw","Yeehaw","Googol","B9","Googol","NSM","B9","NSM","Dont Ask","Googol"])
                      ,  P (map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                           (map Query ["Googol","Dont Ask","NSM","NSM","Yeehaw","Yeehaw","Googol"])])]

  generatorForSet 1 = Just $ do
    s <- QS.choose (2, 10)
    ss <- QS.vectorOf s QS.arbitrary
    q <- QS.choose (2, 100)
    qs <- QS.vectorOf q QS.arbitrary
    return $ P (map SearchEngine ss) (map Query qs)
  generatorForSet 2 = Just $ do
    s <- QS.choose (2, 100)
    ss <- QS.vectorOf s QS.arbitrary
    q <- QS.choose (2, 1000)
    qs <- QS.vectorOf q QS.arbitrary
    return $ P (map SearchEngine ss) (map Query qs)
  generatorForSet _ = Nothing

data S = S Int
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S i) = "Case #" ++ show n ++ ": " ++ show i
  displayExamples = [([S 1, S 0], "Case #1: 1\nCase #2: 0\n")]

solve' :: P -> S
solve' (P _ []) = S 0
solve' (P ss qs) = S $ result 0 ss qs
  where result acc _ [] = acc
        result acc [] qs' = result (acc + 1) ss qs'
        result acc ss' (Query q:qs') = result acc (delete (SearchEngine q) ss') qs'
