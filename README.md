# k-means clustering algorithm

See wiki links:
- k-Means generally: https://en.wikipedia.org/wiki/K-means_clustering#Software_implementations
- Elbow method: https://en.wikipedia.org/wiki/Elbow_method_(clustering)
- Silhouette score: https://en.wikipedia.org/wiki/Silhouette_(clustering)


# How to use

There are two main ways of executing the algorithm. The difference is in how the original centroids are established.

I would recommend using `kMeans`, as shown below. The kMeans function randomizes the points to create the original centroids. Every time the function is called, a different set of centroids will be initialized.

```haskell

import KMeans.Algorithm

-- kMeans :: Point a => Int -> [a] -> IO [Cluster a]

main :: IO ()
main =
 do clusters <- kMeans k points
    putStrLn $ displayClusters clusters
  where
    k = 2 -- How many clusters to create
    points = [(5, 3), (9, 2), (2, 5), (4, 1), (7, 3), (10, 4), (7, 2), (1, 2)]

```

But the library also contains `kMeansStatic`, which allows you to specify a seed for the randomized centroids. This way, you can get the same output from the algorithm every time you call it, which can be helpful for testing.

```haskell

-- kMeansStatic :: Point a => Int -> Int -> [a] -> [Cluster a]

main :: IO ()
main =
    kMeansStatic seed k points >>=
        (putStrLn . displayClusters)
  where
    seed = 6 -- You can try different seeds to get specific outcomes
    k = 2
    points = -- ...

```

# `Point` instances

The library provides the following instances of `Point`:
- `Point (Double, Double)`, intended for coordinates. The distance between two points is the hypotenuse, as determined according to the [Pythagorean theorem](https://en.wikipedia.org/wiki/Pythagorean_theorem).
- `Point Int`, intended mostly to facilitate other instances involving numerical values.
- `Point Char`, again intended to facilitate another instance, in this case `Point Text`.
- `Point Text`, allows clustering of text. Distance is determined according to the [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance).
- `Point [a]`, allows clustering of lists of anything that with an instance of `Point`.

If the library doesn't contain an instance of the `Point` class to your needs, feel free to create one.

