import Test.HUnit (runTestTT, Test(..), (~:), (~?=))
import Control.Monad (void)
import Solution (P(..), S(..), solve')
import GCJ (Problem(..), Solution(..), Solver(..), R(..))

tests :: (Solver r) => r -> Test
tests r =
  let parseExamples'   = parseExamples :: [(String,[P])]
      displayExamples' = displayExamples :: [([S],String)]
  in TestList [ "Can parse"   ~: map (parse . tail . lines . fst) parseExamples' ~?= map snd parseExamples'
              , "Can display" ~: map (unlines . zipWith display [1..] . fst) displayExamples' ~?= map snd displayExamples'
              , "Can solve"   ~: map ((solve r) . tail . lines . fst) parseExamples' ~?= map (lines . snd) displayExamples'
              ]

main :: IO ()
main = void $ runTestTT $ tests (R solve')
