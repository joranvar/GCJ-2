module Solution (P(..), examples, input, problems, output, Solution, display, solve) where
import Data.List (delete)
import GCJ (Problem(..))

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

data Solution = Solution Int
  deriving (Eq, Show)

display :: Solution -> String
display (Solution i) = " " ++ show i

solve :: P -> Solution
solve (P _ []) = Solution 0
solve (P ss qs) = Solution $ result 0 ss qs
  where result acc _ [] = acc
        result acc [] qs' = result (acc + 1) ss qs'
        result acc ss' (Query q:qs') = result acc (delete (SearchEngine q) ss') qs'

data Example = Example { input    :: String
                       , problems :: [(P, Solution)]
                       , output   :: String }

examples :: [Example]
examples =
  [ Example { input = ""
  , output = "Case #1: 1\n\
              \Case #2: 0\n\
              \"
  , problems = [ ( P (map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                      (map Query ["Yeehaw","Yeehaw","Googol","B9","Googol","NSM","B9","NSM","Dont Ask","Googol"])
                 , Solution 1 )
               , ( P (map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                     (map Query ["Googol","Dont Ask","NSM","NSM","Yeehaw","Yeehaw","Googol"])
                 , Solution 0 ) ] } ]
