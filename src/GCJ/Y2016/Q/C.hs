{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..))
import Data.List (unfoldr)

data P = P Int Int
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P (head nj') (head . tail $ nj') : parse rest
    where nj:rest = inp
          nj' = map read $ words nj

  parseExamples = [ ( "1\n6 3\n"
                    , [ P 6 3 ] ) ]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate 16 50
                            , testRuntime = 4 * 60 * 1000
                            , numCases = 1 }
                  , TestSet { name = "Large"
                            , generator = generate 32 500
                            , testRuntime = 8 * 60 * 1000
                            , numCases = 1 } ]
    where generate n j =  do
            return ( P n j , [] )

type Divisor = Integer
type Coin = (Integer, [Divisor])

data S = S [Coin]
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S coins) = init $ "Case #" ++ show n ++ ":\n" ++ unlines (map displayDivisors coins) where
    displayDivisors (i, ds) = show i ++ concatMap (\d -> " " ++ show d) ds
  displayExamples = [( [S [(100011, [5, 13, 147, 31, 43, 1121, 73, 77, 629])
                          ,(111111, [21, 26, 105, 1302, 217, 1032, 513, 13286, 10101])
                          ,(111001, [3, 88, 5, 1938, 7, 208, 3, 20, 11])] ]
                     , "Case #1:\n100011 5 13 147 31 43 1121 73 77 629\n111111 21 26 105 1302 217 1032 513 13286 10101\n111001 3 88 5 1938 7 208 3 20 11\n" )]

data R = R
instance GCJ.Runner R P S where
  solve R (P n j) = S (take j $ mine n) where
    mine :: Int -> [Coin]
    mine len = takeWhile allGood $ map addDivisors $ generate len
    allGood :: Coin -> Bool
    allGood = undefined
    addDivisors :: Integer -> Coin
    addDivisors = undefined
    generate :: Int -> [Integer]
    generate = undefined

  props R =
    [ 
    ]

primes :: [Integer]
primes = (2:) $ concat $ unfoldr (\(p:xs) ->
                                   let (small,large) = span (<p*p) xs in
                                   Just (p:small, [x | x<-large
                                                     , all (\f -> x`mod`f/=0) (p:small)])) [3,5..]
