module Main (main) where

import KMeans.Algorithm (kMeansStatic)
import KMeans.Cluster (displayClusters)

import KMeans.OptimizeK.ElbowMethod


-- Coords

coordinates :: [(Double, Double)]
coordinates = 
    [ (46.67, 1.28)
    , (36.21, 41.47)
    , (91.73, 35.01)
    , (93.26, 37.41)
    , (64.51, 78.7)
    , (44.34, 84.58)
    , (19.4, 1.53)
    {-, (0.37, 16.02)
    , (24.1, 75.05)
    , (71.67, 83.63)
    , (65.16, 71.9)
    , (62.49, 38.76)
    , (12.95, 78.27)
    , (75.84, 91.98)
    , (87.94, 66.87)
    , (56.84, 34.49)
    , (35.24, 76.48)
    , (53.64, 82.27)
    , (37.27, 2.42)
    , (63.62, 52.73)
    , (46.44, 67.09)
    , (37.75, 79.53)
    , (25.41, 97.41)
    , (35.45, 72.9)-}
    , (21.17, 16.91) 
    ]


main :: IO ()
main =
 do putStrLn $ displayClusters $ kMeansStatic 6 3 coordinates
    print $ elbowMethod coordinates
