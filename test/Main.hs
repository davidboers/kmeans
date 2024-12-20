{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import KMeans.Algorithm
import KMeans.Cluster
import KMeans.OptimizeK.ElbowMethod
import KMeans.OptimizeK.Silhouette
import KMeans.Point
import KMeans.Scaling

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import Data.List
import Prelude hiding (unlines)

import System.Directory (listDirectory)
import System.FilePath ((</>))

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
    , (0.37, 16.02)
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
    , (35.45, 72.9)
    , (21.17, 16.91)
    ]

text :: [Named String [T.Text]] -> T.Text
text namedLangs =
    T.unlines $
        map T.pack $
            [ show $ kMeansStatic 6 3 coordinates
            , show $ elbowMethod coordinates
            , show $ silhouetteCoefficient (length coordinates) 6 coordinates
            , show $
                concat (Cluster [[1, 3, 5], [2, 6, 4, 1], [3, 7, 4, 2, 6]] :: Cluster [Int])
            , show $ kMeansStatic 6 3 namedLangs
            , ""
            , "Closest friends:"
            ]
                ++ map (testClosestFriend namedLangs) namedLangs
                ++ [ ""
                   , "Default random points (seed 42):"
                   , show $ initCoords 10 42
                   , ""
                   , "Points plotted on a 2D Cartesian plane:"
                   ]
                ++ map testNamed (plotNamedPoints namedLangs)

testClosestFriend
    :: (Show n, Eq n, Point a) => [Named n a] -> Named n a -> String
testClosestFriend ps p =
    show p ++ ": " ++ show (closestFriend ps p)

testNamed :: (Show a, Point a) => Named String a -> String
testNamed (Named{..}) =
    name ++ ": " ++ show point
testNamed virtual = show virtual

showSteps :: [[(Double, Double)]] -> [String]
showSteps steps =
    [ intercalate
        ","
        [ show stepNum
        , show coordNum
        , show (fst $ steps !! (stepNum - 1) !! (coordNum - 1))
        , show (snd $ steps !! (stepNum - 1) !! (coordNum - 1))
        ]
    | stepNum <- [1 .. 300]
    , coordNum <- [1 .. 25]
    ]

main :: IO ()
main = do
    langNames <- listDirectory langFolder
    langs <- mapM ((fmap getStrings <$> TIO.readFile) . (langFolder </>)) langNames
    let namedLangs = namePoints langNames langs
    TIO.writeFile "test/out.txt" $ text namedLangs
    TIO.putStrLn $ T.unlines $ map T.unwords langs
  where
    langFolder = "test/languages"

    getStrings :: T.Text -> [T.Text]
    getStrings bs = map T.stripEnd $ T.lines bs
