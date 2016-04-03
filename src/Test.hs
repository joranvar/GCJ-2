import Test.HUnit (runTestTT, Test(..), (~:), (~?=))
import Test.QuickCheck (quickCheck, within, forAll, Property)
import Control.Monad (void)
import qualified Data.Maybe as Maybe (fromJust)
import Solution (P(..), S(..), solve')
import GCJ (Problem(..), Solution(..), Runner(..), R(..))

tests :: (Runner r) => r -> Test
tests r =
  let parseExamples'   = parseExamples :: [(String,[P])]
      displayExamples' = displayExamples :: [([S],String)]
  in TestList [ "Can parse"   ~: map (parse . tail . lines . fst) parseExamples' ~?= map snd parseExamples'
              , "Can display" ~: map (unlines . zipWith display [1..] . fst) displayExamples' ~?= map snd displayExamples'
              , "Can solve"   ~: map ((solve r) . fst) parseExamples' ~?= map snd displayExamples'
              ]

checks :: R P S -> [Property]
checks (R solve) = [ (forAll (Maybe.fromJust $ generatorForSet 1) ((/=) "" . display (-1) . solve'))
                   , (forAll (Maybe.fromJust $ generatorForSet 2) ((/=) "" . display (-1) . solve')) ]

limits :: Int
limits =  4 * 60 * 1000 `div` 20

main :: IO ()
main = sequence_ [ void . runTestTT $ tests (R solve')
                 , mapM_ (quickCheck . within limits) $ checks (R solve')
                 ]
