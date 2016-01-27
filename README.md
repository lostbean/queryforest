# Description

This a Haskell library that includes 3 kinds of algorithms for fast search of nearest neighbor and range search in
multidimensional spaces.

## KD-tree

[KD-tree](https://en.wikipedia.org/wiki/K-d_tree) a classical algorithm that creates a partition tree of the space and
perform fast searches in O(log n). But it is limited to vector spaces where the distance function and the projected
distance on linear independent axis are required.

```
class Point p where
  dimension :: p -> Int
  coord     :: Int -> p -> Double
  dist      :: p -> p -> Double
```

## VP-tree and MVP-tree

[VP-tree](https://en.wikipedia.org/wiki/Vantage-point_tree) (Vantage-point tree) and MVP-tree (Multi-Vantage-Point tree)
algorithms are both based on metric trees and they work on metric spaces. Metric spaces only require the definition
of a distance function and, therefore, are a generalization of vector space.

```
class Metric p where
  dist :: p -> p -> Double
```

## References

[1] P. N. Yianilos, “Data Structures and Algorithms for Nearest Neighbor Search in General Metric Spaces,” 1993, pp. 311–321.

[2] E. Hagen, “Using machine learning to balance metric trees,” 2006.
