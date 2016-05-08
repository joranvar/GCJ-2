{-# LANGUAGE MultiParamTypeClasses #-}

module Solution (P(..), S(..), R(..)) where
import GCJ (Problem(..), Solution(..), Runner(..), TestSet(..), limits)
import qualified Test.QuickCheck as QS
import Control.Arrow (second)
import Data.List (groupBy, maximumBy, permutations)
import Data.Ord (comparing)

data P = P Int Int Int Int
  deriving (Eq, Show)
instance GCJ.Problem P where
  parse [] = []
  parse inp = P j p s k : parse rest
    where jpsk:rest = inp
          [j,p,s,k] = map read $ words jpsk

  parseExamples = [ ( "4\n1 1 1 10\n1 2 3 2\n1 1 3 2\n1 2 3 1\n"
                    , [ P 1 1 1 10, P 1 2 3 2, P 1 1 3 2, P 1 2 3 1 ] ) ]

  setGenerators = [ TestSet { name = "Small"
                            , generator = generate 20
                            , testRuntime = 4 * 60 * 1000 * 1000
                            , numCases = 100 }
                  , TestSet { name = "Large"
                            , generator = generate 2000
                            , testRuntime = 8 * 60 * 1000 * 1000
                            , numCases = 100 } ]
    where generate maxLen =  do
            (j, jLabel) <- GCJ.limits 3 maxLen
            (p, pLabel) <- GCJ.limits 3 maxLen
            (s, sLabel) <- GCJ.limits 3 maxLen
            (k, kLabel) <- GCJ.limits 3 maxLen
            return ( P j p s k
                   , map (uncurry (++) . second show) [("j:", jLabel),("p:", pLabel),("s:", sLabel),("k:", kLabel)])

data S = S [(Int,Int,Int)]
  deriving (Eq, Show)
instance GCJ.Solution S where
  display n (S outfits) = "Case #" ++ show n ++ ": " ++ (show $ length outfits) ++ "\n" ++ (init . unlines $ map display' outfits)
    where
      display' (j, s, k) = tail $ concatMap ((" " ++) . show) [j,s,k]
  displayExamples = [( [S [(1,1,1)], S [(1,1,2), (1,2,3), (1,2,1), (1,1,1)], S [(1,1,2), (1,1,1)], S [(1,1,3), (1,2,1)]]
                     , "Case #1: 1\n1 1 1\nCase #2: 4\n1 1 2\n1 2 3\n1 2 1\n1 1 1\nCase #3: 2\n1 1 2\n1 1 1\nCase #4: 2\n1 1 3\n1 2 1\n" )]

data R = R
instance GCJ.Runner R P S where
  solve R (P j p s k) =
    let js = [1..j]
        ps = [1..p]
        ss = [1..s]
        outfits = [(j,p,s) | j <- js, p <- ps, s <- ss]
        jpsets = maximum . map length . groupBy (\(j0,p0,s0) (j1,p1,s1) -> j0==j1 && p0==p1) :: [(Int,Int,Int)] -> Int
        jssets = maximum . map length . groupBy (\(j0,p0,s0) (j1,p1,s1) -> j0==j1 && s0==s1) :: [(Int,Int,Int)] -> Int
        pssets = maximum . map length . groupBy (\(j0,p0,s0) (j1,p1,s1) -> p0==p1 && s0==s1) :: [(Int,Int,Int)] -> Int
        maybeAddOutfit fits fit = if maximum (map ($ fit:fits) [jpsets, jssets, pssets]) <= k then fit:fits else fits
        allowedOutfits = foldl maybeAddOutfit []
    in S $ maximumBy (comparing length) $ map allowedOutfits $ permutations outfits

  props R =
    [ ( "True"
      , \(P j p s k) (S xs) -> True )
    ]
