module KMeans.OptimizeK.Silhouette where

import KMeans.Point
import KMeans.Cluster
import KMeans.Algorithm

import Data.List.Extras.Argmax


silhouetteCoefficient :: Point a => [a] -> Int
silhouetteCoefficient points =
    argmax (sTidle points) [2..length points]

sTidle :: Point a => [a] -> Int -> Double
sTidle points k =
    let clusters = kMeansStatic 100 k points in
    avgD $ map (silhouetteScore clusters) points

silhouetteScore :: Point a => [Cluster a] -> a -> Double
silhouetteScore clusters i =
    let cluster = getCluster clusters i
        a = meanIntraClusterDistance cluster i
        b = meanNearestClusterDistance clusters i in
    (b - a) / max a b

meanIntraClusterDistance :: Point a => Cluster a -> a -> Double
meanIntraClusterDistance c i =
    (1 / fromIntegral (length c - 1)) * sum [ distance i j | j <- toList c, i /= j ]

meanNearestClusterDistance :: Point a => [Cluster a] -> a -> Double
meanNearestClusterDistance clusters i =
    minimum $
        map (avgD . toList . fmap (distance i)) $
        filter (\cluster -> not $ i `KMeans.Cluster.elem` cluster) clusters

avgD :: [Double] -> Double
avgD l =
    sum l / fromIntegral (length l)