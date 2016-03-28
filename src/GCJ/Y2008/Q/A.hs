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
  [ ( [ "5"
      , "Yeehaw"
      , "NSM"
      , "Dont Ask"
      , "B9"
      , "Googol"
      , "10"
      , "Yeehaw"
      , "Yeehaw"
      , "Googol"
      , "B9"
      , "Googol"
      , "NSM"
      , "B9"
      , "NSM"
      , "Dont Ask"
      , "Googol"
      , "5"
      , "Yeehaw"
      , "NSM"
      , "Dont Ask"
      , "B9"
      , "Googol"
      , "7"
      , "Googol"
      , "Dont Ask"
      , "NSM"
      , "NSM"
      , "Yeehaw"
      , "Yeehaw"
      , "Googol" ]
    , [ Problem ["Yeehaw","NSM","Dont Ask","B9","Googol"] ["Yeehaw","Yeehaw","Googol","B9","Googol","NSM","B9","NSM","Dont Ask","Googol"]
      , Problem ["Yeehaw","NSM","Dont Ask","B9","Googol"] ["Googol","Dont Ask","NSM","NSM","Yeehaw","Yeehaw","Googol"] ] ) ]
