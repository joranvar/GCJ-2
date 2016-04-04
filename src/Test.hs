import Test.HUnit (runTestTT, Test(..), (~:), (~?=))
import Test.QuickCheck (quickCheck, within, forAll, Property, label, property)
import Control.Monad (void)
import Solution (P(..), S(..), solve')
import GCJ (Problem(..), Solution(..), Runner(..), R(..), TestSet(..))

tests :: (Runner r) => r -> Test
tests r =
  let parseExamples'   = parseExamples :: [(String,[P])]
      displayExamples' = displayExamples :: [([S],String)]
  in TestList [ "Can parse"   ~: map (parse . tail . lines . fst) parseExamples' ~?= map snd parseExamples'
              , "Can display" ~: map (unlines . zipWith display [1..] . fst) displayExamples' ~?= map snd displayExamples'
              , "Can solve"   ~: map ((solve r) . fst) parseExamples' ~?= map snd displayExamples'
              ]

checks :: R P S -> [Property]
checks (R solve) = map (\testset ->
                           within (testRuntime testset `div` numCases testset)
                           $ label (name testset)
                           $ forAll (generator testset) (\(p, labels) -> (flip . foldr) label labels . property . (/=) "" . display (-1) $ solve p))
                   $ setGenerators

main :: IO ()
main = sequence_ [ void . runTestTT $ tests (R solve')
                 , mapM_ quickCheck $ checks (R solve')
                 ]
