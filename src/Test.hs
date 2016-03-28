import Test.HUnit (runTestTT, Test(..), (~:), (~?=))

import Solution (parse, examples, input, problems, output)

tests = TestList
  [ "Can parse" ~: map (parse . tail . lines . input) examples ~?= map (map fst . problems) examples
  , "Can show"  ~: map (show . map snd . problems) examples ~?= map output examples ]

main = runTestTT tests
