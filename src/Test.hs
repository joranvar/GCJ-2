import Test.HUnit (runTestTT, Test(..), (~:), (~?=))
import Control.Monad (void)
import Solution (examples, input, problems, output, Solution, display, solve)
import GCJ (Problem(..))

showSolution :: Int -> Solution -> String
showSolution i s = "Case #" ++ show i ++ ":" ++ display s

tests :: Test
tests = TestList
  [ "Can parse" ~: map (parse . tail . lines . input) examples ~?= map (map fst . problems) examples
  , "Can show"  ~: map (unlines . zipWith showSolution [1..] . map snd . problems) examples ~?= map output examples
  , "Can solve" ~: map (map (solve . fst) . problems) examples ~?= map (map snd . problems) examples ]

main :: IO ()
main = void $ runTestTT tests
