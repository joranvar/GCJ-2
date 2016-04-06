import Test.HUnit (runTestTT, Test(..), (~:), (~?=))
import Test.QuickCheck (quickCheck, within, forAll, Property, label, property)
import Control.Monad (void)
import Solution (P(..), S(..), R(..))
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..))

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

main :: IO ()
main = sequence_ [ void . runTestTT $ tests R
                 , mapM_ quickCheck $ checks R
                 ]
