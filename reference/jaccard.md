# Jaccard similarity

Jaccard similarity identifies the changes of ties between two matrices.

## Usage

``` r
jaccard(
  A,
  B,
  directed = TRUE,
  diag = FALSE,
  coparticipation = FALSE,
  bipartite = FALSE
)
```

## Arguments

- A:

  Binary matrix A

- B:

  Binary matrix B

- directed:

  Whether the matrix is directed (asymmetric)

- diag:

  Whether the diagonal should be considered

- coparticipation:

  Select nodes that co-participate in both matrices

- bipartite:

  Whether the matrix is incidence

## Value

The output are: `jaccard` = Jaccard similarity, `proportion` =
proportion among the ties present at a given observation of ties that
are also present in the other matrix, and `table` = a table with the tie
changes between matrices.

If `coparticipation = TRUE`, then also: `match` = The number of nodes
present in both matrices; `size_matrix1` = The size of the first matrix;
`size_matrix2` = The size of the second matrix; `coparticipation1` = The
percentage of nodes in the first matrix also present in the second
matrix; `coparticipation2` = The percentage of nodes in the second
matrix also present in the first matrix: `overlap_actors` = Overlap of
nodes between two matrices

If `coparticipation = TRUE` and `bipartite = TRUE`, then also: `matchM1`
= The number of nodes in the first 'mode' present in both matrices;
`matchM2` = The number of nodes in the second 'mode' present in both
matrices; `size_matrix1_M1` = The number of nodes in the first 'mode' of
the first matrix; `size_matrix1_M2` = The number of nodes in the second
'mode' of the first matrix; `size_matrix2_M1` = The number of nodes in
the first 'mode' of the second matrix; `size_matrix2_M2` = The number of
nodes in the second 'mode' of the second matrix; `coparticipation1_M2` =
The percentage of nodes of the first 'mode' in the first matrix present
in the second matrix. `coparticipation1_M2` = The percentage of nodes of
the second 'mode' in the first matrix present in the second matrix.
`coparticipation2_M1` = The percentage of nodes of the first 'mode' in
the second matrix present in the first matrix. `coparticipation2_M2` =
The percentage of nodes of the second 'mode' in the second matrix
present in the first matrix. `overlap_actors_M1` = Overlap between two
matrices (nodes of the first 'mode') `overlap_actors_M2` = Overlap
between two matrices (nodes of the second 'mode')

## References

Batagelj, V., and Bren, M. (1995). Comparing resemblance measures.
Journal of Classification 12, 73–90.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0,
  1, 0, 0, 0,
  1, 0, 0, 0,
  0, 0, 1, 0
), byrow = TRUE, ncol = 4)
B <- matrix(c(
  0, 1, 1, 0,
  1, 0, 0, 0,
  1, 0, 0, 0,
  0, 0, 0, 0
), byrow = TRUE, ncol = 4)
jaccard(A, B, directed = TRUE)
#> $jaccard
#> [1] 0.8
#> 
#> $proportion
#> [1] 0.8
#> 
#> $table
#>       B
#> A      0 1 <NA>
#>   0    7 0    0
#>   1    1 4    0
#>   <NA> 0 0    0
#> 
```
