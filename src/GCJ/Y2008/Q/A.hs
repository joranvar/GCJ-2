module Solution (A(..), examples, input, problems, output, Solution, display, solve) where
import Data.List (delete)
import GCJ (Problem(..))

newtype SearchEngine = SearchEngine String deriving (Eq, Show)
newtype Query        = Query String        deriving (Eq, Show)

data A = A [SearchEngine] [Query]
  deriving (Eq, Show)
instance GCJ.Problem A where
  parse [] = []
  parse inp = A (map SearchEngine ss) (map Query qs) : parse rest
    where s:spart     = inp
          (ss, qpart) = splitAt (read s) spart
          q:qpart'    = qpart
          (qs, rest)  = splitAt (read q) qpart'
  parseExamples = [("2\n\
                     \5\n\
                     \Yeehaw\n\
                     \NSM\n\
                     \Dont Ask\n\
                     \B9\n\
                     \Googol\n\
                     \10\n\
                     \Yeehaw\n\
                     \Yeehaw\n\
                     \Googol\n\
                     \B9\n\
                     \Googol\n\
                     \NSM\n\
                     \B9\n\
                     \NSM\n\
                     \Dont Ask\n\
                     \Googol\n\
                     \5\n\
                     \Yeehaw\n\
                     \NSM\n\
                     \Dont Ask\n\
                     \B9\n\
                     \Googol\n\
                     \7\n\
                     \Googol\n\
                     \Dont Ask\n\
                     \NSM\n\
                     \NSM\n\
                     \Yeehaw\n\
                     \Yeehaw\n\
                     \Googol\n\
                     \", [  A (map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                              (map Query ["Yeehaw","Yeehaw","Googol","B9","Googol","NSM","B9","NSM","Dont Ask","Googol"])
                         ,  A (map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                              (map Query ["Googol","Dont Ask","NSM","NSM","Yeehaw","Yeehaw","Googol"])])]

data Solution = Solution Int
  deriving (Eq, Show)

display :: Solution -> String
display (Solution i) = " " ++ show i

solve :: A -> Solution
solve (A _ []) = Solution 0
solve (A ss qs) = Solution $ result 0 ss qs
  where result acc _ [] = acc
        result acc [] qs' = result (acc + 1) ss qs'
        result acc ss' (Query q:qs') = result acc (delete (SearchEngine q) ss') qs'

data Example = Example { input    :: String
                       , problems :: [(A, Solution)]
                       , output   :: String }

examples :: [Example]
examples =
  [ Example { input = ""
  , output = "Case #1: 1\n\
              \Case #2: 0\n\
              \"
  , problems = [ ( A (map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                     (map Query ["Yeehaw","Yeehaw","Googol","B9","Googol","NSM","B9","NSM","Dont Ask","Googol"])
                 , Solution 1 )
               , ( A (map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                     (map Query ["Googol","Dont Ask","NSM","NSM","Yeehaw","Yeehaw","Googol"])
                 , Solution 0 ) ] } ]
