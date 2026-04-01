# Structural balance

Structural balance

## Usage

``` r
struc_balance(A, B = NULL, score = c("triangle", "walk"))
```

## Arguments

- A:

  A signed symmetric matrix (i.e., with ties that are either -1, 0 or 1)

- B:

  A signed symmetric matrix considered as the negative ties (i.e., with
  ties that are either -1, 0 or 1)

- score:

  Whether to return the `triangle` (default) or `walk` balance score
  (Aref and Wilson, 2017)

## Value

This function return the structural balance (Heider, 1940; Cartwright
and Harary, 1956). When `B` is used, matrix A is considered the negative
matrix and `A` the positive matrix.

## References

Aref, Samin and Wilson, Mark C. (2017). Measuring partial balance in
signed networks. Journal of Complex Networks, 6(4): 566-595.

Cartwright, Dorwin, and Harary, Frank (1956). Structural balance: a
generalization of Heider's theory. Psychological review, 63(5), 277.

Heider, Fritz (1946). Attitudes and Cognitive Organization. The Journal
of Psychology, 21: 107–112

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, -1, -1, 0,
  -1, 0, 1, 0,
  -1, 1, 0, 0,
  0, 0, 0, 0
), byrow = TRUE, ncol = 4)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)
struc_balance(A)
#> $table
#>   sign1 sign2 sign3 number balance
#> 1    -1    -1     1      3     --+
#> 
#> $balance_score
#> [1] 1
#> 
```
