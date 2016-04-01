module Solution (parse, examples, input, problems, output, Solution, display, solve) where

import Data.List (delete)

newtype SearchEngine = SearchEngine String deriving (Eq, Show)
newtype Query        = Query String        deriving (Eq, Show)

data Problem = Problem [SearchEngine] [Query]
  deriving (Eq, Show)

data Solution = Solution Int
  deriving (Eq, Show)

parse :: [String] -> [Problem]
parse []    = []
parse inp = Problem (map SearchEngine ss) (map Query qs) : parse rest
  where s:spart     = inp
        (ss, qpart) = splitAt (read s) spart
        q:qpart'    = qpart
        (qs, rest)  = splitAt (read q) qpart'

display :: Solution -> String
display (Solution i) = " " ++ show i

solve :: Problem -> Solution
solve (Problem _ []) = Solution 0
solve (Problem ss qs) = Solution $ result 0 ss qs
  where result acc _ [] = acc
        result acc [] qs' = result (acc + 1) ss qs'
        result acc ss' (Query q:qs') = result acc (delete (SearchEngine q) ss') qs'

data Example = Example { input    :: String
                       , problems :: [(Problem, Solution)]
                       , output   :: String }

examples :: [Example]
examples =
  [ Example { input = "2\n\
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
                       \"
  , output = "Case #1: 1\n\
              \Case #2: 0\n\
              \"
  , problems = [ ( Problem (map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                           (map Query ["Yeehaw","Yeehaw","Googol","B9","Googol","NSM","B9","NSM","Dont Ask","Googol"])
                 , Solution 1 )
               , ( Problem (map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                           (map Query ["Googol","Dont Ask","NSM","NSM","Yeehaw","Yeehaw","Googol"])
                 , Solution 0 ) ] } ]
