module Solution (parse, examples) where

type SearchEngine = String
type Query = String

data Problem = Problem [SearchEngine] [Query]
  deriving (Eq, Show)

parse :: [String] -> [Problem]
parse [] = []
parse input = Problem ss qs : parse rest
      where s:spart = input
            (ss, qpart) = splitAt (read s) spart
            q:qpart' = qpart
            (qs, rest) = splitAt (read q) qpart'

examples :: [([String], [Problem])]
examples =
  [ (lines "5\n\
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
           \Googol"
    , [ Problem ["Yeehaw","NSM","Dont Ask","B9","Googol"] ["Yeehaw","Yeehaw","Googol","B9","Googol","NSM","B9","NSM","Dont Ask","Googol"]
      , Problem ["Yeehaw","NSM","Dont Ask","B9","Googol"] ["Googol","Dont Ask","NSM","NSM","Yeehaw","Yeehaw","Googol"]
      ]) ]
