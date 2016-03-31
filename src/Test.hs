import Test.HUnit (runTestTT, Test(..), (~:), (~?=))

import Solution (parse, examples, input, problems, output, Solution)

showSolution :: Int -> Solution -> String
showSolution i s = "Case #" ++ show i ++ ":" ++ show s

tests = TestList
  [ "Can parse" ~: map (parse . tail . lines . input) examples ~?= map (map fst . problems) examples
  , "Can show"  ~: map (unlines . zipWith showSolution [1..] . map snd . problems) examples ~?= map output examples ]

main = runTestTT tests
