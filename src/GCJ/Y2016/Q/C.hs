{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..))
import Data.Bits (bit, shift)
import Data.Digits (digits, unDigits)
import Data.List (unfoldr, find, sortOn)
import Data.Maybe (catMaybes, isNothing, fromJust)
import Control.Arrow (second)

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
                            , testRuntime = 4 * 60 * 1000 * 1000
                            , numCases = 1 }
                  , TestSet { name = "Large"
                            , generator = generate 32 500
                            , testRuntime = 8 * 60 * 1000 * 1000
                            , numCases = 1 } ]
    where generate n j =  do
            return ( P n j , [] )

type Divisor = Integer
type Coin = (Integer, [Divisor])
type CoinProspect = (Integer, [Maybe Divisor])

data S = S [Coin]
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S coins) = init $ "Case #" ++ show n ++ ":\n" ++ unlines (map displayDivisors coins) where
    displayDivisors (i, ds) = showBinary i ++ concatMap (\d -> " " ++ show d) ds
    showBinary i = concatMap show $ digits 2 i
  displayExamples = [( [S [((unDigits 2) [1,0,0,0,1,1], [5, 13, 147, 31, 43, 1121, 73, 77, 629])
                          ,((unDigits 2) [1,1,1,1,1,1], [21, 26, 105, 1302, 217, 1032, 513, 13286, 10101])
                          ,((unDigits 2) [1,1,1,0,0,1], [3, 88, 5, 1938, 7, 208, 3, 20, 11])] ]
                     , "Case #1:\n100011 5 13 147 31 43 1121 73 77 629\n111111 21 26 105 1302 217 1032 513 13286 10101\n111001 3 88 5 1938 7 208 3 20 11\n" )]

data R = R
instance GCJ.Runner R P S where
  solve R (P n j) = S (take j $ mine) where
    mine :: [Coin]
    mine =
      map (second catMaybes)
      $ reverse $ sortOn (length . catMaybes . snd)
      $ fromJust
      $ find haveEnough
      $ scanl (\prospects prime -> map (addDivisor prime) prospects) (take 10000 allCoinProspects) primes

    haveEnough :: [CoinProspect] -> Bool
    haveEnough = (==j) . length . take j . filter ((==9) . length . catMaybes . snd)

    addDivisor :: Integer -> CoinProspect -> CoinProspect
    addDivisor d (i, ds) =
      ( i
      , zipWith (\base md -> if isNothing md && dividesInBase base i d
                             then Just d
                             else md) [2..] ds )

    dividesInBase :: Int -> Integer -> Integer -> Bool
    dividesInBase base x divisor = (x `toBase` base) `mod` divisor == 0

    allCoinProspects :: [CoinProspect]
    allCoinProspects = map (\i -> (i, replicate 9 Nothing)) $ generate n

    generate :: Int -> [Integer]
    generate len = map (toCoinProspect len) [0..(bit (max 0 $ len-2) - 1)]

    toCoinProspect :: Int -> Integer -> Integer
    toCoinProspect len filling = (bit (max 0 $ len-1)) + (shift filling 1) + 1

  props R =
    [ ( "j is honored"
      , \(P _ j) (S coins) -> length coins == j )
    , ( "Length of coins is honored"
      , \(P n _) (S coins) -> all (\(coin, _) -> length (digits 2 coin) == n ) coins )
    , ( "Number of bases is honored"
      , \_ (S coins) -> all ((==9) . length . snd) coins)
    , ( "All divisors"
      , \_ (S coins) ->
        all (\(coin, divisors) ->
              all (\(base, divisor) -> toBase coin base `mod` divisor == 0)
              $ zip [2..] divisors) coins )
    ]

primes :: [Integer]
primes = (2:) $ concat $ unfoldr (\(p:xs) ->
                                   let (small,large) = span (<p*p) xs in
                                   Just (p:small, [x | x<-large
                                                     , all (\f -> x`mod`f/=0) (p:small)])) [3,5..]

toBase :: Integer -> Int -> Integer
toBase i base = unDigits (fromIntegral base) $ digits 2 i
