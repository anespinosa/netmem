# Transitivity

This measure is sometimes called clustering coefficient.

## Usage

``` r
trans_coef(
  A,
  method = c("weakcensus", "global", "mean", "local"),
  select = c("all", "in", "out")
)
```

## Arguments

- A:

  A matrix

- method:

  Whether to calculate the `weakcensus`, `global` transitivity ratio,
  the `mean` transitivity or the `local` transitivity.

- select:

  Whether to consider `all`, `in` or `out` ties for the local
  transitivity.

## Value

Return a transitivity measure

## References

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 0, 1, 0,
  1, 0, 1, 1, 0,
  0, 1, 0, 0, 0,
  1, 1, 0, 0, 1,
  0, 0, 0, 1, 0
), byrow = TRUE, ncol = 5)
rownames(A) <- letters[1:ncol(A)]
colnames(A) <- rownames(A)

trans_coef(A, method = "local")
#> $a
#> [1] 1
#> 
#> $b
#> [1] 0.3333333
#> 
#> $c
#> [1] NaN
#> 
#> $d
#> [1] 0.3333333
#> 
#> $e
#> [1] NaN
#> 
```
