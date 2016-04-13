import Test.HUnit (runTestTT, Test(..), (~:), (~?=), Counts(..))
import Test.QuickCheck (quickCheckResult, within, forAll, Property, label, property)
import Test.QuickCheck.Test (isSuccess)
import Control.Monad (unless)
import Solution (P(..), S(..), R(..))
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..))
import System.Exit (exitFailure)

unittests :: R -> Test
unittests r =
  let parseExamples'   = parseExamples :: [(String,[P])]
      displayExamples' = displayExamples :: [([S],String)]
  in TestList [ "Can parse"   ~: map (parse . tail . lines . fst) parseExamples' ~?= map snd parseExamples'
              , "Can display" ~: map (unlines . zipWith display [1..] . fst) displayExamples' ~?= map snd displayExamples'
              , "Can solve"   ~: map (interactor r . fst) parseExamples' ~?= map snd displayExamples'
              ]

checks :: R -> [Property]
checks r =
  map (\testset ->
        within (testRuntime testset `div` numCases testset)
        $ label (name testset)
        $ forAll (generator testset) (\(p, labels) ->
                                       let s = solve r p
                                           results = map (\(propertyName, test) ->
                                                           let result = test p s in
                                                           (result, if result then [propertyName] else []))
                                             $ props r
                                       in
                                         (flip . foldr) label (labels ++ concatMap snd results) .
                                         property $ all fst results ) )
  $ setGenerators

main :: IO ()
main = do
  testResults <- runTestTT $ unittests R
  checkResults <- mapM quickCheckResult $ checks R
  unless ( errors testResults == 0 &&
           all isSuccess checkResults ) exitFailure
