module KMeans.Algorithm (kMeans, kMeansStatic) where

import KMeans.Point
import KMeans.Centroid
import KMeans.Cluster

import System.Random

import Data.List.Extras.Argmax


maxIter :: Int
maxIter = 100


-- Call algorithm

getKMeans :: Point a => Int -> Int -> [a] -> [Centroid a] -> [Cluster a]
getKMeans triesLeft k points centroids =
  let clusters = foldl (makeClusters centroids) (replicate k (Cluster [])) points
      nc = map newCentroid clusters
  in
  if centroids == nc || triesLeft == 0
    then clusters
    else getKMeans (triesLeft - 1) k points nc

-- | @'kMeans' k ps@ creates @k@ clusters from the points in list @ps@. The kMeans 
-- function randomizes the points to create the original centroids. Every time the 
-- function is called, a different set of centroids will be initialized.
kMeans :: Point a => Int -> [a] -> IO [Cluster a]
kMeans k points =
 do rands <- mapM (\_ -> randomRIO (0, length points - 1)) [0..k]
    return $ getKMeans maxIter k points $ initializeCentroids rands k points

-- | @'kMeansStatic' seed k ps@ creates @k@ clusters from points in list @ps@. The
-- kMeans function allows users to specify a @seed@ for the randomized centroids. 
-- This way, the same output from the algorithm can be obtained every time it is
-- called, which can be helpful for testing.
kMeansStatic :: Point a => Int -> Int -> [a] -> [Cluster a]
kMeansStatic seed k points =
    getKMeans maxIter k points $ initializeCentroids rands k points
  where
    rands = randomRs (0, length points - 1) (mkStdGen seed)

initializeCentroids :: Point a => [Int] -> Int -> [a] -> [Centroid a]
initializeCentroids _      0 _      = []
initializeCentroids []     _ _      = []
initializeCentroids (r:rs) k points =
    Centroid (points !! r)
      : initializeCentroids rs (k-1) points


-- Procedure

makeClusters :: Point a => [Centroid a] -> [Cluster a] -> a -> [Cluster a]
makeClusters centroids accum p =
    let clusterAssignment = argminIndex (distance2Centroid p) centroids in
    prependInList clusterAssignment p accum
  where
    argminIndex :: Ord b => (a -> b) -> [a] -> Int
    argminIndex func l =
        argmin (func . (!!) l) [0..length l - 1]

prependInList :: Point a => Int -> a -> [Cluster a] -> [Cluster a]
prependInList i new l =
  case splitAt i l of
    (before, (Cluster nth):after) -> before ++ Cluster (new : nth) : after
    (before, [])                  -> before ++ [Cluster [new]]