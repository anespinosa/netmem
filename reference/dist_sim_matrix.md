# Structural similarities

In the literature of social network, Euclidean distance (Burt, 1976) or
correlations (Wasserman and Faust, 1994) were considered as measures of
structural equivalence.

## Usage

``` r
dist_sim_matrix(
  A,
  method = c("euclidean", "hamming", "jaccard"),
  bipartite = FALSE
)
```

## Arguments

- A:

  A matrix

- method:

  The similarities/distance currently available are either `Euclidean`
  (default), `Hamming`, or `Jaccard`.

- bipartite:

  Whether the object is an incidence matrix

## Value

This function returns a distance matrix between nodes of the same
matrix.

## References

Burt, Ronald S. (1976) Positions in networks. Social Forces, 55(1):
93-122.

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 0, 0, 1,
  0, 0, 0, 1, 1,
  0, 1, 0, 0, 1,
  0, 0, 1, 1, 0,
  0, 1, 0, 0, 0
), nrow = 5, ncol = 5, byrow = TRUE)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)
dist_sim_matrix(A, method = "jaccard")
#>           [,1]      [,2]      [,3]      [,4] [,5]
#> [1,] 0.0000000 0.6666667 0.0000000 1.0000000  0.5
#> [2,] 0.6666667 0.0000000 0.6666667 0.6666667  1.0
#> [3,] 0.0000000 0.6666667 0.0000000 1.0000000  0.5
#> [4,] 1.0000000 0.6666667 1.0000000 0.0000000  1.0
#> [5,] 0.5000000 1.0000000 0.5000000 1.0000000  0.0

A <- matrix(c(
  0, 0, 3, 0, 5,
  0, 0, 2, 0, 4,
  5, 4, 0, 4, 0,
  0, 3, 0, 1, 0,
  0, 0, 0, 0, 2
), nrow = 5, ncol = 5, byrow = TRUE)
dist_sim_matrix(A, method = "euclidean")
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 0.000000 1.414214 9.539392 6.633250 4.242641
#> [2,] 1.414214 0.000000 8.774964 5.477226 2.828427
#> [3,] 9.539392 8.774964 0.000000 5.916080 7.810250
#> [4,] 6.633250 5.477226 5.916080 0.000000 3.741657
#> [5,] 4.242641 2.828427 7.810250 3.741657 0.000000
```
