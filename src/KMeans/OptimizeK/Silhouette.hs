module KMeans.OptimizeK.Silhouette (silhouetteCoefficient, meanSilhouetteScore, silhouetteScore, meanIntraClusterDistance, meanNearestClusterDistance) where

import KMeans.Point
import KMeans.Cluster
import KMeans.Algorithm

import Data.List.Extras.Argmax

-- | @'silhouetteCoefficient' maxK seed ps@ returns the optimal @k@ between 2 and
-- @length ps@. The coefficient is the @k@ that provides the maximum 
-- 'meanSilhouetteScore'. 
--
-- \(SC = \max_k \tilde{s}\left(k\right)\)
--
-- In Haskell:
--
-- > silhouetteCoefficient maxK seed ps == argmax (meanSilhouetteScore seed ps) [2..maxK]
--
-- ==== __Examples__
-- 
-- >>> let points = [1, 3, 5, 19, 3, 6, 5, 8, 2]
-- >>> silhouetteScore (length points) 100 points
-- 
-- Because the function iteratively performs the k-means algorithm, processing large
-- datasets significantly increase runtime. It is recommended that the function be
-- called a single time, rather than before each calling of the 'kMeans' function.
-- Furthermore, as it is rare for more than three or four clusters to be optimal, the
-- user can provide a smaller @maxK@. For example, if @length points >= 50@:
--
-- >>> let maxK = fromIntegral $ length points / 2.0
-- >>> silhouetteScore maxK 100 points
silhouetteCoefficient :: Point a => Int -> Int -> [a] -> Int
silhouetteCoefficient maxK seed points =
    argmax (meanSilhouetteScore seed points) [2..maxK]

-- | @'meanSilhouetteScore' seed ps k@ returns the average Silhouette score of points 
-- @ps@ sorted into @k@ clusters. 
-- 
-- This function uses the 'kMeansStatic' function to cluster the @ps@. The given @seed@
-- is used to initialize the centroids.
meanSilhouetteScore :: Point a => Int -> [a] -> Int -> Double
meanSilhouetteScore seed points k =
    let clusters = kMeansStatic seed k points in
    avgD $ map (silhouetteScore clusters) points

-- | @'silhouetteScore' clusters p@ returns the Silhouette score for a single point @p@.
--
-- @length clusters >= 1@. If there is a single cluster, 0 is returned. If @length clusters > 1@: 
--
-- \(s(i) = \frac{b(i) - a(i)}{\max\{a(i),b(i)\}}\)
--
-- where \(i\) is the index of @p@, \(a(i)\) is the 'meanIntraClusterDistance' of @p@,
-- and \(b(i)\) is the 'meanNearestClusterDistance' of @p@.
silhouetteScore :: Point a => [Cluster a] -> a -> Double
silhouetteScore [_]      _ = 0 
silhouetteScore clusters p =
    let cluster = getCluster clusters p
        a = meanIntraClusterDistance cluster p
        b = meanNearestClusterDistance clusters p in
    (b - a) / max a b

-- | @'meanIntraClusterDistance' c p@ returns the mean distance between @p@ and all 
-- other data points in the same cluster @c@. 
--
-- \(a(i) = \frac{1}{|C_I| - 1} \sum_{j \in C_I, i \neq j} d(i, j)\)
--
-- where \(C_I\) is @length c@, \(i\) is the index of @p@ within @c@, and \(d(i,j)\)
-- is the 'distance' function.
--
-- In Haskell:
--
-- > meanIntraClusterDistance c p = (1 / fromIntegral (length c - 1)) * sum [ distance p j | j <- toList c, p /= j ]
--
-- ==== __Examples__
--
-- Using 'getCluster':
-- 
-- >>> let c = getCluster clusters p
-- >>> meanIntraClusterDistance c p
meanIntraClusterDistance :: Point a => Cluster a -> a -> Double
meanIntraClusterDistance c p =
    (1 / fromIntegral (length c - 1)) * sum [ distance p j | j <- toList c, p /= j ]

-- | @'meanNearestClusterDistance' clusters p@ returns the smallest mean distance of
-- point @p@ to all other points in any other cluster in @clusters@. The cluster with 
-- this smallest mean dissimilarity is said to be the "neighboring cluster" of @p@ 
-- because it is the next best fit cluster for point @p@. 
--
-- \(b(i) = \min_{J \neq I} \frac{1}{|C_J|} \sum_{j \in C_J} d(i, j)\)
--
-- where \(C_J\) is the length of cluster \(J\), \(I\) is the index of the cluster
-- containing @p@, and \(d(i,j)\) is the 'distance' function.
meanNearestClusterDistance :: Point a => [Cluster a] -> a -> Double
meanNearestClusterDistance clusters p =
    minimum $
        map (avgD . toList . fmap (distance p)) $
        filter (\cluster -> not $ p `KMeans.Cluster.elem` cluster) clusters

avgD :: [Double] -> Double
avgD l =
    sum l / fromIntegral (length l)