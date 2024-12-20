{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{- |

The `Cluster` data type.
-}
module KMeans.Cluster
    ( Cluster (..)
    , sortCluster
    , sortClusterByDistance
    , getCluster
    , toList
    , elem
    , emptyCluster
    ) where

import KMeans.Point
import KMeans.Utils

import Data.Char
import Data.Foldable (Foldable (..))
import Data.List

{- | Wrapper type for a list of any type constrained by 'Point'. Variables of this
wrapper type are produced by the clustering algorithm.

Although the type wrapper is not constrained by 'Point', it is not recommended that
a non-'Point' type be put in a cluster. In general, functions in this library
impose the constraint when a cluster is passed to, or returned from, a function.

Instances of 'Functor', 'Foldable', and 'Traversable' exist, allowing for clusters
to be handled in a similar way to lists:

>>> concat (Cluster [[1, 3, 5], [2, 6, 4, 1], [3, 7, 4, 2, 6]])
[1,3,5,2,6,4,1,3,7,4,2,6]
-}
newtype Cluster a = Cluster [a]

instance Functor Cluster where
    fmap f (Cluster ps) = Cluster $ fmap f ps

instance Foldable Cluster where
    foldMap f (Cluster ps) = foldMap f ps

    -- \| @'toList' c@ Returns the list of points in cluster @c@.
    --
    -- > toList (Cluster ps) == ps
    toList (Cluster ps) = ps

instance Traversable Cluster where
    traverse f (Cluster ps) = Cluster <$> traverse f ps

instance (Point a, Show a) => Show (Cluster a) where
    show cluster = unlines $ map show (toList cluster)

    -- \| Creates a multiline string, displaying the clustered points. Each cluster is
    -- assigned a letter, A through Z.
    showList clusters =
        (++) (unlines $ mapIndex showListCluster $ map sortClusterByDistance clusters)

showListCluster :: (Point a, Show a) => Int -> Cluster a -> String
showListCluster i =
    showString ("Cluster " ++ [c] ++ ":\n") . show
  where
    c = chr (ord 'A' + i)

{- | @'emptyCluster'@ returns a cluster with no points.

> emptyCluster == Cluster []
-}
emptyCluster :: Cluster a
emptyCluster = Cluster []

-- Utils

{- | @'sortCluster' x@ sorts the points in cluster @x@. @a@ must be an instance of 'Ord'.
A new 'Cluster' of the sorted 'Point's is returned.

> sortCluster (Cluster points) == Cluster (sort points)
-}
sortCluster :: Point a => Ord a => Cluster a -> Cluster a
sortCluster (Cluster points) =
    Cluster $ sort points

{- | @'sortClusterByDistance' c@ sorts the points in cluster @x@ according to how close
each point is to the centroid. Points closer to the centroid (more typical of the
cluster) appear first in the sorted cluster.

> sortClusterByDistance (Cluster points) = Cluster (sortOn (`distance2Centroid` centroid)) points
-}
sortClusterByDistance :: Point a => Cluster a -> Cluster a
sortClusterByDistance (Cluster points) =
    Cluster $ sortOn (distance centroid) points
  where
    centroid = center points

{- | @'getCluster' cs p@ returns the 'Cluster' of point @p@, provided it is in the
list @cs@. If, for some reason, the @p@ is found in multiple clusters, the cluster
that appears first in @cs@ is returned. If @p@ appears in no clusters, an exception
is thrown.

> getCluster cs p == head $ filter (\c -> p `elem` c) cs
-}
getCluster :: Point a => [Cluster a] -> a -> Cluster a
getCluster clusters point =
    head $ filter (\c -> point `elem` c) clusters
