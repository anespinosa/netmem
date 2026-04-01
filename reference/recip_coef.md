# Reciprocity

This measure calculated the reciprocity of an asymmetric matrix
(directed graph).

## Usage

``` r
recip_coef(
  A,
  diag = NULL,
  method = c("total_ratio", "ratio_nonnull", "global")
)
```

## Arguments

- A:

  A matrix

- diag:

  Whether to consider the diagonal of the matrix

- method:

  Whether to use `total_ratio`, `ratio_nonnull` or `global` reciprocity

## Value

Return a reciprocity coefficient

## References

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(0, 1, 1, 0,
              1, 0, 1, 0,
              0, 0, 0, 0,
              1, 0, 0, 0), byrow = TRUE, ncol = 4)
recip_coef(A)
#> Mutual 
#>    0.5 
```
