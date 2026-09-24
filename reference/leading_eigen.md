# Communities with the leading eigenvector

Groups of nodes that are connected among themselves more often than
expected, found with the leading eigenvector of the modularity matrix
(Newman, 2006).

## Usage

``` r
leading_eigen(A, digraph = FALSE, weighted = FALSE, max_groups = NULL)
```

## Arguments

- A:

  A square matrix

- digraph:

  Whether the matrix is directed or undirected

- weighted:

  Whether the matrix is weighted

- max_groups:

  Maximum number of groups

## Value

This function returns the group of each node, the number of groups and
the modularity of the partition.

## Details

The modularity matrix is \\B = A - kk^T / 2m\\, the observed ties minus
the ties expected from the degrees. The sign of its leading eigenvector
splits the network into two groups, and each group is split again while
the modularity of the partition increases.

## References

Newman, M. E. J. (2006). Finding community structure in networks using
the eigenvectors of matrices. Physical Review E, 74(3), 036104.
[doi:10.1103/PhysRevE.74.036104](https://doi.org/10.1103/PhysRevE.74.036104)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 0, 0, 0,
  1, 1, 0, 1, 0, 0,
  0, 0, 1, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  0, 0, 0, 1, 1, 0
), byrow = TRUE, ncol = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

leading_eigen(A)
#> $partition
#> a b c d e f 
#> 2 2 2 1 1 1 
#> 
#> $groups
#> [1] 2
#> 
#> $modularity
#> [1] 0.3571429
#> 
```
