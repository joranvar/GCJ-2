import Test.HUnit (runTestTT, Test(..), (~:), (~?=))

import Solution (parse, examples)

tests = TestList
  [ "Can parse" ~: map (parse . fst) examples ~?= map snd examples ]

main = runTestTT tests
