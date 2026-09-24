# Transitivity

This measure is sometimes called clustering coefficient.

## Usage

``` r
trans_coef(
  A,
  method = c("weakcensus", "global", "mean", "local", "barrat"),
  select = c("all", "in", "out")
)
```

## Arguments

- A:

  A matrix

- method:

  Whether to calculate the `weakcensus`, `global` transitivity ratio,
  the `mean` transitivity, the `local` transitivity or the weighted
  transitivity of `barrat`.

- select:

  Whether to consider `all`, `in` or `out` ties for the local
  transitivity.

## Value

Return a transitivity measure

## References

Barrat, A., Barthelemy, M., Pastor-Satorras, R. and Vespignani, A.
(2004). The architecture of complex weighted networks. Proceedings of
the National Academy of Sciences, 101(11), 3747–3752.
[doi:10.1073/pnas.0400087101](https://doi.org/10.1073/pnas.0400087101)

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

# The weighted transitivity of Barrat et al. (2004) weighs each triangle
# by the strength of the two ties of the node
W <- matrix(c(
  0, 4, 0, 2, 0,
  4, 0, 1, 3, 0,
  0, 1, 0, 0, 0,
  2, 3, 0, 0, 5,
  0, 0, 0, 5, 0
), byrow = TRUE, ncol = 5)
trans_coef(W, method = "barrat")
#> [1] 1.0000 0.4375     NA 0.2500     NA
```
