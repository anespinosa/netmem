# Leiden communities

Communities found with the algorithm of Traag, Waltman and van Eck
(2019), which improves the algorithm of Louvain by guaranteeing that the
groups it returns are internally connected.

## Usage

``` r
leiden(
  A,
  resolution = 1,
  objective = c("modularity", "cpm"),
  theta = 0.01,
  iterations = 10,
  weighted = FALSE,
  refine = TRUE
)
```

## Arguments

- A:

  A symmetric matrix object

- resolution:

  Resolution of the quality function. Higher values give smaller groups

- objective:

  Quality function: `modularity` (default) or `cpm`

- theta:

  How random the refinement is. Small values only accept the best merges

- iterations:

  Maximum number of times the three phases are repeated

- weighted:

  Whether the matrix is weighted

- refine:

  Whether the groups are refined before the aggregation. Without the
  refinement the algorithm is the one of Louvain (Blondel et al., 2008),
  which can return groups that are internally disconnected

## Value

This function returns the group of each node, the number of groups, and
the modularity and the quality of the partition.

## Details

The algorithm has three phases that are repeated until the partition no
longer changes. First, every node is moved to the neighbouring group
that improves the quality of the partition the most, while any move
improves it. Second, each group is refined: within the group, the nodes
start as singletons and are merged at random among the merges that
improve the quality, which is what keeps the groups connected. Third,
the refined groups become the nodes of an aggregated network, and the
process starts again on it.

Two quality functions are available. The `modularity` compares the ties
within groups with the ties expected from the degrees, and suffers from
a resolution limit. The constant Potts model (`cpm`) compares them with
a constant density given by `resolution`, so the groups it finds do not
depend on the size of the network.

The refinement is random, so the result depends on the seed.

## References

Blondel, V. D., Guillaume, J.-L., Lambiotte, R. and Lefebvre, E. (2008).
Fast unfolding of communities in large networks. Journal of Statistical
Mechanics, 2008(10), P10008.
[doi:10.1088/1742-5468/2008/10/P10008](https://doi.org/10.1088/1742-5468/2008/10/P10008)

Traag, V. A., Waltman, L. and van Eck, N. J. (2019). From Louvain to
Leiden: guaranteeing well-connected communities. Scientific Reports, 9,
5233.
[doi:10.1038/s41598-019-41695-z](https://doi.org/10.1038/s41598-019-41695-z)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(0, 12, 12)
A[1:4, 1:4] <- 1
A[5:8, 5:8] <- 1
A[9:12, 9:12] <- 1
diag(A) <- 0
A[4, 5] <- 1
A[5, 4] <- 1
A[8, 9] <- 1
A[9, 8] <- 1
rownames(A) <- letters[1:12]
colnames(A) <- rownames(A)

set.seed(18051889)
leiden(A)
#> $partition
#> a b c d e f g h i j k l 
#> 1 1 1 1 2 2 2 2 3 3 3 3 
#> 
#> $groups
#> [1] 3
#> 
#> $modularity
#> [1] 0.56625
#> 
#> $quality
#> [1] 0.56625
#> 
```
