import Test.HUnit (runTestTT, Test(..), (~:), (~?=))
import Control.Monad (void)
import Solution (P(..), examples, input, problems, output, S(..), display, solve)
import GCJ (Problem(..))

showSolution :: Int -> S -> String
showSolution i s = "Case #" ++ show i ++ ":" ++ display s

tests :: Test
tests =
  let parseExamples' = parseExamples :: [(String,[P])]
  in TestList [ "Can parse" ~: map (parse . tail . lines . fst) parseExamples' ~?= map snd parseExamples'
              , "Can show"  ~: map (unlines . zipWith showSolution [1..] . map snd . problems) examples ~?= map output examples
              , "Can solve" ~: map (map (solve . fst) . problems) examples ~?= map (map snd . problems) examples ]

main :: IO ()
main = void $ runTestTT tests
