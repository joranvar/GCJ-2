import Test.HUnit (runTestTT, Test(..), (~:), (~?=))

import Solution (parse, examples, input, problems)

tests = TestList
  [ "Can parse" ~: map (parse . tail . lines . input) examples ~?= map (map fst . problems) examples ]

main = runTestTT tests
