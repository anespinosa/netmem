# Preserved order

Whether a centrality index preserves a dominance relation.

## Usage

``` r
preserved_order(
  P,
  scores,
  tol = sqrt(.Machine$double.eps),
  direction = c("dominated", "dominates")
)
```

## Arguments

- P:

  A binary dominance matrix

- scores:

  A vector of centrality scores in the same order as the rows of `P`

- tol:

  Numerical tolerance

- direction:

  Whether `P[u, v] = 1` means that `u` is `dominated` by `v` (default)
  or that `u` `dominates` `v`

## Value

This function returns whether the order is preserved and the pairs that
violate it.

## Details

A centrality index preserves the dominance if no dominated node has a
higher score than the node that dominates it (Schoch and Brandes, 2016).
Differences smaller than `tol` are considered ties, as eigenvectors and
other iterative scores are only approximated.

## References

Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in
social networks. European Journal of Applied Mathematics, 27(6),
971–985.
[doi:10.1017/S0956792516000401](https://doi.org/10.1017/S0956792516000401)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 1, 0,
  1, 0, 0, 0, 0,
  1, 0, 0, 0, 1,
  1, 0, 0, 0, 0,
  0, 0, 1, 0, 0
), byrow = TRUE, ncol = 5)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

P <- neigh_inclusion(A)
preserved_order(P, rowSums(A))
#> $preserved
#> [1] TRUE
#> 
#> $violations
#> [1] dominated        dominating       score_dominated  score_dominating
#> <0 rows> (or 0-length row.names)
#> 
preserved_order(P, betweenness_centrality(A, digraph = FALSE))
#> $preserved
#> [1] TRUE
#> 
#> $violations
#> [1] dominated        dominating       score_dominated  score_dominating
#> <0 rows> (or 0-length row.names)
#> 
```
