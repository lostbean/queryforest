# queryforest

Spatial indexing and nearest-neighbor search for metric spaces.

## What is this?

When you have a large set of points and need to answer questions like "which points are within radius *r* of this query point?" or "which point is closest?", brute-force linear scans become impractical. `queryforest` provides tree-based data structures that organize points hierarchically so these queries can skip large portions of the dataset.

The library provides four implementations, each suited to different scenarios.

## Data structures

### KD-tree

The classic space-partitioning tree for coordinate-based vector spaces (2D, 3D, etc.). Points are recursively split along alternating coordinate axes using the median as the pivot.

**How it works:** At each level, the tree picks one coordinate axis (cycling through dimensions) and splits the points at the median value. To find nearest neighbors, the search descends the tree and prunes entire branches when the splitting hyperplane is farther than the current best distance.

**Best for:** Low-dimensional coordinate data (2D/3D) where you can access individual coordinates. Performance degrades in high dimensions (the "curse of dimensionality").

### VP-tree (Vantage-Point tree)

A metric tree that works in any space where you can compute a distance -- no coordinates required. Based on the paper by P. N. Yianilos (1993).

**How it works:** A vantage point is selected, and all other points are partitioned into "close" and "far" groups based on their distance to the vantage point (split at the median distance). Queries exploit the triangle inequality: if the query point is very far from the vantage point, the "close" branch can be skipped entirely, and vice versa.

**Best for:** General metric spaces where you only have a distance function (e.g., angular distance between quaternions, edit distance between strings).

### BlazeVPtree (high-performance VP-tree)

Same algorithm as the VP-tree, but stores the entire tree in a single flat unboxed vector instead of an algebraic tree structure. Construction is done in-place using mutable arrays, and queries navigate via index arithmetic rather than pointer-chasing.

**How it works:** The tree is laid out in a contiguous array where each node sits at the midpoint of its index range. The left half is the "close" branch, the right half is the "far" branch. The farthest point is selected as vantage point (maximizing spread), and partial sorting (introsort) finds the median without fully sorting the array.

**Best for:** Performance-critical applications where the point type can be stored in unboxed vectors. Same algorithmic complexity as VP-tree but with better cache locality and lower memory overhead.

### MVP-tree (Multi-Vantage-Point tree)

Uses two vantage points per node, creating four-way partitions instead of two-way. Each point stores cached distances to both vantage points, enabling more aggressive pruning.

**How it works:** At each node, two vantage points (VP1 and VP2) are selected. Points are first split by distance to VP1 (close/far), then each half is split again by distance to VP2, yielding four child subtrees. During queries, up to three of the four branches can be pruned using the triangle inequality against both vantage points.

**Best for:** High-dimensional data where distance computation is expensive (e.g., earth-mover distance, protein structure similarity). The extra vantage point means fewer distance computations per query, at the cost of more complex tree structure.

## Which one should I use?

| If you have... | Use |
|---|---|
| 2D/3D points with coordinates | KD-tree |
| Any metric space, simplicity preferred | VP-tree |
| Any metric space, maximum performance needed | BlazeVPtree |
| Expensive distance function, high dimensions | MVP-tree |

## Example

```haskell
import qualified Data.Vector.Unboxed as U
import qualified Data.BlazeVPtree as BVP

type V3 = (Double, Double, Double)

instance BVP.Metric V3 where
    dist (a1, a2, a3) (b1, b2, b3) =
        sqrt $ (b1-a1)^2 + (b2-a2)^2 + (b3-a3)^2

main :: IO ()
main = do
    let points = U.fromList [(0,0,0), (1,0,0), (0,1,0), (1,1,0), (0.5,0.5,0)] :: U.Vector V3
        tree   = BVP.fromVector points
        query  = (0.1, 0.1, 0.0) :: V3

    print $ BVP.nearNeighbors tree 0.5 query     -- all points within radius 0.5
    print $ BVP.nearestNeighbor tree query        -- single closest point
```

## Where is it used?

- **sledge** -- crystallographic texture analysis uses BlazeVPtree to index quaternion orientations on the 3-sphere and efficiently find neighbors within an angular distance for kernel density estimation of orientation distribution functions.

## How to build

```bash
# With Nix (recommended)
nix develop
cabal build --allow-newer

# With Cabal
cabal build

# Run tests
cabal test

# Run benchmarks (brute force vs VP-tree vs BlazeVPtree on 40,000 random 3D points)
cabal bench
```

## References

- P. N. Yianilos, "Data Structures and Algorithms for Nearest Neighbor Search in General Metric Spaces," 1993.
- E. Hagen, "Using machine learning to balance metric trees," 2006.

## License

MIT -- see [LICENSE](./LICENSE).
