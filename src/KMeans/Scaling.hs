module KMeans.Scaling (plotPoints, initCoords, smacof, distanceMatrix, smacofSteps) where

import           System.Random

import           KMeans.Point
import           KMeans.Utils


-- | @'distanceMatrix ps'@ creates a matrix of the distances between each
-- point in @ps@. This function can be used to obtain the distance between
-- each point and every other point.
distanceMatrix :: Point a => [a] -> [[Double]]
distanceMatrix ps =
    [[ distance x y | y <- ps ] | x <- ps ]

stress :: [[Double]] -> [(Double, Double)] -> Double
stress distances coords =
    let coordDeltas = distanceMatrix coords in
    sum [ (distances   !! i !! j) -
          (coordDeltas !! i !! j) ** 2
        | i <- indices coords
        , j <- indices coords
        , i < j
        ]

smacofStep :: [[Double]] -> [(Double, Double)] -> [(Double, Double)]
smacofStep distances coords =
    [ ( avgDouble [ matrixB i j * fst (coords !! j) | j <- m ]
      , avgDouble [ matrixB i j * snd (coords !! j) | j <- m ]
      )
    | i <- m
    ]
  where
    m = indices coords

    matrixX = distanceMatrix coords

    matrixB i j
        | i == j    = sum [ weight i j1 | j1 <- m, i /= j1 ]
        | otherwise = -(weight i j)

    weight i j = (distances !! i !! j) / (matrixX !! i !! j + 1e-2)

-- | @'smacof' d triesLeft x@ Creates a set of coordinates on a 2D Cartesian plane
-- where the distance between any 2 points is approximately the same as the distance
-- provided in matrix @d@ for the two respective points. A native implementation of
-- the SMACOF (Scaling by Majorizing a Complicated Function) method is used. This
-- function is exposed to allow for customized usage. The 'plotPoints' method is
-- recommended instead. The algorithm will continue calculating until @triesLeft@ is
-- 0, or until the marginal gain of continuing (as indicated by the loss function),
-- fails under @1e-3@. An initialized set of coords must be provided: @x@. The
-- 'initCoords' function is recommended for this purpose.
smacof :: [[Double]] -> Int -> [(Double, Double)] -> [(Double, Double)]
smacof _         0         coords = coords
smacof distances triesLeft coords
    | relStress < 1e-2 = nextCoords
    | otherwise        = smacof distances (triesLeft - 1) nextCoords
  where
    nextCoords = smacofStep distances coords
    nextStress = stress distances nextCoords
    thisStress = stress distances coords
    relStress = abs (thisStress - nextStress)

-- | @'initCoords' n seed@ creates a set of @n@ coordinates, each randomized according
-- to @seed@ for usage with the 'smacof' function.
initCoords :: Int -> Int -> [(Double, Double)]
initCoords n seed =
    uncurry zip $
    splitAt n $
    map fromInteger $
    take (n * 2) $
    randomRs (1, 10) (mkStdGen seed)

smacofSteps :: [[Double]] -> Int -> [(Double, Double)] -> [[(Double, Double)]]
smacofSteps _         0         coords = [coords]
smacofSteps distances triesLeft coords =
    let step = smacofStep distances coords in
    step : smacofSteps distances (triesLeft - 1) step

-- | @'plotPoints' ps@ creates a set of coordinates, each representing one of the given
-- points @ps@, on a 2D Cartesian plane. This function is an alias for 'smacof', which
-- can be used to customize the algorithm. This function can be used to visualize a set
-- of points, even if the Point type doesn't lend itself to visual presentation. For more
-- information on multidimensional scaling, see [here](https://en.wikipedia.org/wiki/Multidimensional_scaling)
--
-- > plotPoints ps == smacof (distanceMatrix ps) 300 (initCoords (length ps) 42)
plotPoints :: Point a => [a] -> [(Double, Double)]
plotPoints ps =
    smacof (distanceMatrix ps) 300 (initCoords (length ps) 42)
