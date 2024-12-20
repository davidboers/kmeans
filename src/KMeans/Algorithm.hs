module KMeans.Algorithm (kMeans, kMeansStatic) where

import KMeans.Centroid
import KMeans.Cluster
import KMeans.Point
import KMeans.Utils

import System.Random

import Data.List.Extras.Argmax

maxIter :: Int
maxIter = 300

-- Call algorithm

getKMeans :: Point a => Int -> Int -> [a] -> [Centroid a] -> [Cluster a]
getKMeans triesLeft k points centroids
    | centroids == nc || triesLeft == 0 = clusters
    | otherwise = getKMeans (triesLeft - 1) k points nc
  where
    clusters = foldl (makeClusters centroids) (replicate k emptyCluster) points
    nc = map newCentroid clusters

{- | @'kMeans' k ps@ creates @k@ clusters from the points in list @ps@. The kMeans
function randomizes the points to create the original centroids. Every time the
function is called, a different set of centroids will be initialized.
-}
kMeans :: Point a => Int -> [a] -> IO [Cluster a]
kMeans k points =
    do
        rands <- mapM (const $ randomRIO (0, length points - 1)) [0 .. k]
        let centroids = initializeCentroids points rands
        return $ getKMeans maxIter k points centroids

{- | @'kMeansStatic' seed k ps@ creates @k@ clusters from points in list @ps@. The
kMeans function allows users to specify a @seed@ for the randomized centroids.
This way, the same output from the algorithm can be obtained every time it is
called, which can be helpful for testing.
-}
kMeansStatic :: Point a => Int -> Int -> [a] -> [Cluster a]
kMeansStatic seed k points =
    getKMeans maxIter k points $
        initializeCentroids points $
            take k $
                randomRs (0, length points - 1) (mkStdGen seed)

initializeCentroids :: Point a => [a] -> [Int] -> [Centroid a]
initializeCentroids points =
    map (\r -> Centroid $ points !! r)

-- Procedure

makeClusters :: Point a => [Centroid a] -> [Cluster a] -> a -> [Cluster a]
makeClusters centroids accum p =
    prependInList (assignCluster centroids p) p accum

assignCluster :: Point a => [Centroid a] -> a -> Int
assignCluster centroids p =
    argmin (\k -> distance2Centroid p $ centroids !! k) (indices centroids)

prependInList :: Point a => Int -> a -> [Cluster a] -> [Cluster a]
prependInList i new l =
    case splitAt i l of
        (before, (Cluster nth) : after) -> before ++ Cluster (new : nth) : after
        (before, []) -> before ++ [Cluster [new]]
