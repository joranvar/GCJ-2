import Test.HUnit (runTestTT, Test(..), (~:), (~?=))
import Control.Monad (void)
import Solution (P(..), examples, input, problems, output, S(..), solve')
import GCJ (Problem(..), Solution(..))

tests :: Test
tests =
  let parseExamples'   = parseExamples :: [(String,[P])]
      displayExamples' = displayExamples :: [([S],String)]
  in TestList [ "Can parse"   ~: map (parse . tail . lines . fst) parseExamples' ~?= map snd parseExamples'
              , "Can display" ~: map (unlines . zipWith display [1..] . fst) displayExamples' ~?= map snd displayExamples'
              , "Can solve"   ~: map (map (solve' . fst) . problems) examples ~?= map (map snd . problems) examples ]

main :: IO ()
main = void $ runTestTT tests
