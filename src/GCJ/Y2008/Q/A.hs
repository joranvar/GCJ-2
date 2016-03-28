module Solution (parse, examples) where

newtype SearchEngine = SearchEngine String deriving (Eq, Show)
newtype Query        = Query String        deriving (Eq, Show)

data Problem = Problem [SearchEngine] [Query]
  deriving (Eq, Show)

parse :: [String] -> [Problem]
parse []    = []
parse input = Problem (map SearchEngine ss) (map Query qs) : parse rest
  where s:spart     = input
        (ss, qpart) = splitAt (read s) spart
        q:qpart'    = qpart
        (qs, rest)  = splitAt (read q) qpart'

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
    , [ Problem (map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                (map Query ["Yeehaw","Yeehaw","Googol","B9","Googol","NSM","B9","NSM","Dont Ask","Googol"])
      , Problem (map SearchEngine ["Yeehaw","NSM","Dont Ask","B9","Googol"])
                (map Query ["Googol","Dont Ask","NSM","NSM","Yeehaw","Yeehaw","Googol"]) ] ) ]
