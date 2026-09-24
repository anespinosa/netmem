# CONCOR

Convergence of iterated correlations (Breiger, Boorman and Arabie,
1975), a partition of the nodes into positions of structurally
equivalent actors.

## Usage

``` r
concor(A, splits = 1, max_iter = 50, tol = 1e-08)
```

## Arguments

- A:

  A square matrix, or a list of matrices of the same order for multiple
  relations

- splits:

  Number of successive divisions

- max_iter:

  Maximum number of iterated correlations

- tol:

  Tolerance to decide that the correlations have converged

## Value

This function returns the position of each node and the number of
positions.

## Details

The rows and the columns of the matrix describe how each node relates to
the others, so they are stacked into a profile. The correlations between
the profiles of every pair of nodes are computed, and the correlations
of those correlations are computed again and again. The matrix converges
to a matrix of ones and minus ones, which splits the nodes into two
positions. The procedure is repeated within each position, so `splits`
divisions give at most \\2^{splits}\\ positions.

## References

Breiger, R. L., Boorman, S. A. and Arabie, P. (1975). An algorithm for
clustering relational data with applications to social network analysis
and comparison with multidimensional scaling. Journal of Mathematical
Psychology, 12(3), 328–383.
[doi:10.1016/0022-2496(75)90028-0](https://doi.org/10.1016/0022-2496%2875%2990028-0)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 0, 0, 0,
  1, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  0, 0, 0, 1, 1, 0
), byrow = TRUE, ncol = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

concor(A, splits = 1)
#> $partition
#> a b c d e f 
#> 1 1 1 2 2 2 
#> 
#> $positions
#> [1] 2
#> 
```
