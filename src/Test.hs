import Test.HUnit (runTestTT, Test(..), (~:), (~?=), Counts(..))
import Test.QuickCheck (quickCheckResult, within, forAll, Property, label, property)
import Test.QuickCheck.Test (isSuccess)
import Control.Monad (unless)
import Solution (P(..), S(..), R(..))
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..))
import System.Exit (exitFailure)

tests :: R -> Test
tests r =
  let parseExamples'   = parseExamples :: [(String,[P])]
      displayExamples' = displayExamples :: [([S],String)]
  in TestList [ "Can parse"   ~: map (parse . tail . lines . fst) parseExamples' ~?= map snd parseExamples'
              , "Can display" ~: map (unlines . zipWith display [1..] . fst) displayExamples' ~?= map snd displayExamples'
              , "Can solve"   ~: map (interactor r . fst) parseExamples' ~?= map snd displayExamples'
              ]

checks :: R -> [Property]
checks r = map (\testset ->
                 within (testRuntime testset `div` numCases testset)
                 $ label (name testset)
                 $ forAll (generator testset) (\(p, labels) -> (flip . foldr) label labels . property . (/=) "" . display (-1) $ solve r p)) setGenerators

checkProperties :: R -> [Property]
checkProperties r = map (\(name, test) -> label name $ forAll gen (\(p, _) -> test p (solve r p))) $ props r
  where gen = generator $ head setGenerators

main :: IO ()
main = do
  testResults <- runTestTT $ tests R
  checkResults <- mapM quickCheckResult $ checks R
  propertyResults <- mapM quickCheckResult $ checkProperties R
  unless ( errors testResults == 0 &&
           all isSuccess checkResults &&
           all isSuccess propertyResults ) exitFailure
