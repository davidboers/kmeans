{- |

The purpose of this module is to find the optimal amount of clusters @k@ to divide
a group of points into. Finding the optimal @k@  is a crucial step to ensure that
the clustering results are meaningful and useful. See [this Wikipedia
article](https://en.wikipedia.org/wiki/K-means_clustering#Optimal_number_of_clusters)
for more information.
-}
module KMeans.OptimizeK
    ( module KMeans.OptimizeK.ElbowMethod
    , module KMeans.OptimizeK.Silhouette
    ) where

import KMeans.OptimizeK.ElbowMethod
import KMeans.OptimizeK.Silhouette
