# queryforest

A Haskell library providing efficient spatial indexing and nearest-neighbor search data structures for both coordinate-based vector spaces and general metric spaces.

## Overview

`queryforest` implements four tree-based data structures for spatial queries: a KD-tree for coordinate-indexed vector spaces, two Vantage-Point tree variants for general metric spaces, and a Multi-Vantage-Point tree that reduces distance computations in high-dimensional settings.

All implementations support range queries (find all points within a radius) and most support nearest-neighbor queries. The library is designed for performance, making extensive use of unboxed vectors, in-place sorting, and strict evaluation.

## Modules

### Data.KDtree

A classic KD-tree implementation for vector spaces. Points are recursively partitioned along alternating coordinate axes, with the median element chosen as the pivot at each level.

**Type class:**

```haskell
class Point p where
    -- | Number of coordinates (dimensionality).
    dimension :: p -> Int

    -- | Get the k'th coordinate, starting from 0.
    coord :: Int -> p -> Double

    -- | Distance between two points (default: L1 / Manhattan distance).
    dist :: p -> p -> Double
```

Note: the default `dist` implementation computes the sum of absolute coordinate differences (L1 norm), not Euclidean distance. Override it if you need a different metric.

**Data type:**

```haskell
data KdTree point
    = KdNode
        { kdLeft  :: KdTree point
        , kdPoint :: point
        , kdRef   :: Int        -- original index in the input vector
        , kdRight :: KdTree point
        , kdAxis  :: Int        -- splitting axis at this node
        }
    | KdEmpty
```

The tree is a `Functor` and `Foldable`.

**Key functions:**

| Function | Signature | Description |
|---|---|---|
| `fromVector` | `Vector v p => v p -> KdTree p` | Build a KD-tree from a vector of points |
| `nearestNeighbor` | `KdTree p -> p -> Maybe (Int, p)` | Find the single closest point |
| `nearNeighbors` | `KdTree p -> Double -> p -> [(Int, p, Double)]` | Find all points within a given radius |
| `subtrees` | `KdTree p -> [KdTree p]` | List all subtrees including empty leaves |

**Algorithm:** Construction sorts points by the current axis coordinate, picks the median as pivot, and recurses on each half with the next axis (cycling through dimensions). Nearest-neighbor search prunes branches when the splitting hyperplane is farther than the current best distance. Range search prunes branches when the query sphere does not intersect the splitting hyperplane.

### Data.VPtree

A Vantage-Point tree for general metric spaces. Only requires a distance function -- no coordinate access needed.

**Type class:**

```haskell
class Metric p where
    -- | Returns the distance between two points in the metric space.
    dist :: p -> p -> Double
```

**Data type:**

```haskell
data VpTree point
    = VpNode
        { vpInBranch  :: VpTree point   -- points closer than the median distance
        , vpOutBranch :: VpTree point   -- points farther than the median distance
        , vpPoint     :: point          -- the vantage point
        , vpRef       :: !Int           -- original index
        , vpMu        :: !Double        -- median distance (splitting threshold)
        }
    | VpEmpty
```

**Key functions:**

| Function | Signature | Description |
|---|---|---|
| `fromVector` | `v p -> VpTree p` | Build a VP-tree from a vector of points |
| `nearNeighbors` | `VpTree p -> Double -> p -> [(Int, p, Double)]` | Range query: all points within radius |
| `nearestNeighbor` | `VpTree p -> p -> Maybe (Int, p, Double)` | Find the single closest point |
| `nearestThanNeighbor` | `VpTree p -> Double -> p -> Maybe (Int, p, Double)` | Nearest neighbor with a starting radius hint for faster pruning |
| `toList` | `VpTree p -> [(Int, p)]` | Flatten tree to a list of (index, point) pairs |
| `flatTree` | `VpTree p -> [(Int, p, Double)]` | Flatten tree including median distances |

**Algorithm:** A vantage point is selected (the last element in the current set). Distances from all other points to the vantage point are computed, and the set is split at the median distance into an "in" branch (closer) and "out" branch (farther). Range queries exploit the triangle inequality: if the query point's distance to the vantage point minus the search radius exceeds the median, the inner branch can be skipped entirely (and vice versa).

### Data.BlazeVPtree

A high-performance VP-tree implementation that stores the entire tree in a single flat unboxed vector instead of an algebraic tree structure. Construction is performed in-place using the `ST` monad, and queries operate via index arithmetic on the flat array.

**Type class:** Same `Metric` class as `Data.VPtree` (redefined locally in this module).

**Data type:**

```haskell
data VPtree point = VPtree
    { vptree :: U.Vector (Int, point, Double)
    }
```

The tree is stored as an unboxed vector of `(original_index, point, median_distance)` tuples. Each node is located at the midpoint of its index range, with the left half forming the inner branch and the right half forming the outer branch.

**Key functions:**

| Function | Signature | Description |
|---|---|---|
| `fromVector` | `U.Vector a -> VPtree a` | Build tree (requires `Unbox` constraint) |
| `nearNeighbors` | `VPtree p -> Double -> p -> [(Int, p, Double)]` | Range query |
| `nearestNeighbor` | `VPtree p -> p -> Maybe (Int, p, Double)` | Nearest neighbor |
| `nearestThanNeighbor` | `VPtree p -> Double -> p -> Maybe (Int, p, Double)` | Nearest neighbor with starting radius hint |

**Algorithm:** Construction works in-place on a mutable vector. At each step, the farthest point from the current set is selected as the vantage point (maximizing spread). Points are partially sorted by distance to the vantage point using an introsort, and the array is partitioned around the median. The vantage point is placed at the midpoint index with its median distance recorded. Queries navigate the implicit binary tree by computing midpoints of index ranges.

**Performance characteristics:**
- Cache-friendly: contiguous memory layout with no pointer chasing
- Lower memory overhead: no tree node allocations, just one flat vector
- In-place construction: no intermediate allocations during tree building
- Partial sorting: only sorts enough to find the median, avoiding full sort cost

### Data.MVPtree

A Multi-Vantage-Point tree that uses two vantage points per node, creating four-way partitions. This reduces the number of distance computations needed during queries by leveraging distance information from multiple reference points.

**Type class:** Same `Metric` class (redefined locally).

**Data type:**

```haskell
data VPtree point
    = VpNode
        { nodeSS1L :: VPtree point   -- close to VP1, close to VP2
        , nodeMa   :: Double          -- VP2 median for VP1's inner set
        , nodeSS1U :: VPtree point   -- close to VP1, far from VP2
        , nodeMb   :: Double          -- VP1 median distance
        , nodeSS2L :: VPtree point   -- far from VP1, close to VP2
        , nodeMc   :: Double          -- VP2 median for VP1's outer set
        , nodeSS2U :: VPtree point   -- far from VP1, far from VP2
        , nodeVP1  :: point
        , nodeVP2  :: point
        , nodeIx1  :: Int
        , nodeIx2  :: Int
        }
    | VpLeaf { ... }        -- leaf with two vantage points and extra cached distances
    | VpSingleLeaf { ... }  -- single-point leaf
    | VpEmpty
```

**Key functions:**

| Function | Signature | Description |
|---|---|---|
| `fromVector` | `v p -> IO (VPtree p, Path)` | Build tree (returns IO due to MVar-based path tracking) |
| `nearNeighbors` | `(VPtree p, Path) -> Double -> p -> [(Int, p, Double)]` | Range query using the tree and its path data |

Note: `nearestNeighbor` is not yet implemented for the MVP-tree.

**Algorithm:** Each internal node selects two vantage points (VP1 randomly, VP2 as the farthest point from VP1 in the upper partition). Points are first split by median distance to VP1, then each half is further split by median distance to VP2, yielding four child subtrees. During range queries, up to three of the four branches can be pruned using the triangle inequality against both vantage points.

### Data.TreeTools (internal)

Shared utilities used by VP-tree and MVP-tree implementations:

- `popRandom`: Remove a random element from a vector
- `popAt`: Remove an element at a specific index
- `cutMedian`: Split a vector at the median distance value
- `sortSet`: Sort a vector of `(index, point, distance)` triples by distance
- `selectVP`: Heuristic vantage-point selection that maximizes distance variance

## Usage Example

```haskell
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Vector.Unboxed as U
import qualified Data.BlazeVPtree as BVP
import qualified Data.VPtree as VP

type V3 = (Double, Double, Double)

-- Implement the Metric type class for 3D Euclidean distance
instance VP.Metric V3 where
    dist (a1, a2, a3) (b1, b2, b3) =
        let x = b1 - a1
            y = b2 - a2
            z = b3 - a3
        in  sqrt $ x * x + y * y + z * z

instance BVP.Metric V3 where
    dist (a1, a2, a3) (b1, b2, b3) =
        let x = b1 - a1
            y = b2 - a2
            z = b3 - a3
        in  sqrt $ x * x + y * y + z * z

main :: IO ()
main = do
    let points = U.fromList [(0,0,0), (1,0,0), (0,1,0), (1,1,0), (0.5,0.5,0)] :: U.Vector V3
        query  = (0.1, 0.1, 0.0) :: V3
        radius = 0.5

    -- VP-tree
    let vp = VP.fromVector points
    print $ VP.nearNeighbors vp radius query

    print $ VP.nearestNeighbor vp query

    -- BlazeVP-tree (faster, requires Unbox)
    let bvp = BVP.fromVector points
    print $ BVP.nearNeighbors bvp radius query
    print $ BVP.nearestNeighbor bvp query

    -- Nearest neighbor with a starting radius hint (speeds up pruning)
    print $ BVP.nearestThanNeighbor bvp 1.0 query
```

For KD-trees, implement the `Point` type class instead:

```haskell
import Data.KDtree

data Point3D = Point3D !Double !Double !Double

instance Point Point3D where
    dimension _ = 3
    coord 0 (Point3D x _ _) = x
    coord 1 (Point3D _ y _) = y
    coord 2 (Point3D _ _ z) = z
    dist a b = sqrt $ sum [let d = coord i a - coord i b in d*d | i <- [0..2]]

main :: IO ()
main = do
    let points = [Point3D 0 0 0, Point3D 1 0 0, Point3D 0 1 0]
        tree   = fromVector (V.fromList points)
        query  = Point3D 0.1 0.1 0.0

    print $ nearestNeighbor tree query

    print $ nearNeighbors tree 0.5 query
```

## Implementation Selection Guide

| Implementation | Best For | Space Type | Construction | Query Types | Memory Layout |
|---|---|---|---|---|---|
| **KD-tree** | Low-dimensional coordinate data (2D/3D) | Vector space (`Point`) | O(n log n) | nearest, range | Algebraic tree |
| **VP-tree** | General metric spaces, moderate data | Metric space (`Metric`) | O(n log n) | nearest, range | Algebraic tree |
| **BlazeVPtree** | Performance-critical metric queries | Metric space (`Metric` + `Unbox`) | O(n log n), in-place | nearest, range | Flat unboxed vector |
| **MVP-tree** | High-dimensional data, expensive distance functions | Metric space (`Metric` + `Unbox`) | O(n log n), IO | range only | Algebraic tree + Path |

**Rules of thumb:**
- If your points have coordinates and dimensionality is low (2-3D), use **KD-tree**.
- If you only have a distance function, start with **VP-tree** for simplicity.
- If you need maximum query performance and your point type is `Unbox`-able, use **BlazeVPtree**.
- If distance computation is very expensive (e.g., edit distance on strings, earth-mover distance), consider **MVP-tree** to minimize the number of distance evaluations.

## Return Value Convention

All query functions return the original index of each point (its position in the input vector), along with the point value and (for range queries) the computed distance:

- Nearest neighbor: `Maybe (Int, point)` (KD-tree) or `Maybe (Int, point, Double)` (VP-trees)
- Range query: `[(Int, point, Double)]` -- list of (index, point, distance) triples

## Dependencies

- `base >= 4.7`
- `vector >= 0.10` -- boxed, unboxed, and generic vectors
- `vector-algorithms >= 0.6` -- introsort and partial sort
- `random >= 1.0` -- random vantage point selection

## Building

```bash
# With Stack
stack build

# With Cabal
cabal build

# Run tests (validates VP-tree and BlazeVP-tree against brute force on 40,000 random 3D points)
stack test

# Run benchmarks (compares brute force, VP-tree, and BlazeVP-tree on 10,000 queries over 40,000 points)
stack bench
```

## References

[1] P. N. Yianilos, "Data Structures and Algorithms for Nearest Neighbor Search in General Metric Spaces," 1993, pp. 311-321.

[2] E. Hagen, "Using machine learning to balance metric trees," 2006.

## Author

Edgar Gomes de Araujo (<talktoedgar@gmail.com>)

## License

MIT -- see [LICENSE](./LICENSE).
