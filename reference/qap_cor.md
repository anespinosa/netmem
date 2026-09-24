# QAP correlation

Correlation between two matrices, with a test based on the permutation
of the nodes (Hubert and Schultz, 1976; Krackhardt, 1987).

## Usage

``` r
qap_cor(
  A,
  B,
  reps = 1000,
  diag = FALSE,
  method = c("pearson", "spearman", "kendall")
)
```

## Arguments

- A:

  A square matrix

- B:

  A square matrix of the same order

- reps:

  Number of permutations

- diag:

  Whether the diagonal is considered

- method:

  Correlation coefficient: `pearson` (default), `spearman` or `kendall`

## Value

This function returns the observed correlation and the proportion of
permutations with a correlation greater or equal, lower or equal, and
larger in absolute value, than the observed one.

## Details

The ties of a network are not independent, so the usual test of a
correlation does not apply. The quadratic assignment procedure compares
the observed correlation with the correlations obtained after permuting
the rows and the columns of one of the matrices at the same time, which
keeps its structure while breaking its association with the other
matrix.

## References

Hubert, L. and Schultz, J. (1976). Quadratic assignment as a general
data analysis strategy. British Journal of Mathematical and Statistical
Psychology, 29(2), 190–241.
[doi:10.1111/j.2044-8317.1976.tb00714.x](https://doi.org/10.1111/j.2044-8317.1976.tb00714.x)

Krackhardt, D. (1987). QAP partialling as a test of spuriousness. Social
Networks, 9(2), 171–186.
[doi:10.1016/0378-8733(87)90012-8](https://doi.org/10.1016/0378-8733%2887%2990012-8)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0,
  1, 0, 1, 0,
  1, 1, 0, 1,
  0, 0, 1, 0
), byrow = TRUE, ncol = 4)
B <- matrix(c(
  0, 1, 0, 0,
  1, 0, 1, 0,
  0, 1, 0, 1,
  0, 0, 1, 0
), byrow = TRUE, ncol = 4)

set.seed(18051889)
qap_cor(A, B, reps = 100)
#> $correlation
#> [1] 0.7071068
#> 
#> $p_greater
#> [1] 0.22
#> 
#> $p_lower
#> [1] 1
#> 
#> $p_two_sided
#> [1] 0.38
#> 
#> $distribution
#>   [1]  0.0000000  0.0000000  0.0000000  0.7071068  0.7071068  0.0000000
#>   [7]  0.0000000  0.0000000  0.7071068  0.7071068  0.0000000  0.0000000
#>  [13] -0.7071068 -0.7071068 -0.7071068  0.0000000  0.0000000 -0.7071068
#>  [19]  0.0000000  0.7071068 -0.7071068  0.0000000  0.0000000  0.7071068
#>  [25]  0.0000000  0.7071068  0.0000000  0.0000000 -0.7071068  0.0000000
#>  [31] -0.7071068  0.0000000  0.0000000 -0.7071068  0.0000000  0.0000000
#>  [37]  0.0000000  0.0000000  0.7071068  0.0000000  0.0000000 -0.7071068
#>  [43]  0.0000000 -0.7071068 -0.7071068  0.0000000  0.0000000  0.7071068
#>  [49]  0.7071068  0.7071068  0.0000000  0.0000000  0.0000000  0.0000000
#>  [55] -0.7071068  0.0000000  0.7071068  0.7071068  0.7071068  0.0000000
#>  [61]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.7071068
#>  [67]  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000  0.0000000
#>  [73] -0.7071068  0.7071068  0.0000000  0.7071068  0.0000000  0.0000000
#>  [79]  0.0000000 -0.7071068  0.0000000  0.0000000  0.7071068 -0.7071068
#>  [85]  0.0000000  0.7071068  0.0000000  0.7071068  0.7071068  0.0000000
#>  [91]  0.0000000 -0.7071068  0.0000000  0.0000000  0.0000000  0.7071068
#>  [97]  0.0000000  0.0000000  0.0000000  0.0000000
#> 
```
