# Dominance pairs

Comparable and incomparable pairs of a dominance relation.

## Usage

``` r
dominance_pairs(P, direction = c("dominated", "dominates"))
```

## Arguments

- P:

  A binary dominance matrix (e.g. from
  [`neigh_inclusion()`](https://anespinosa.github.io/netmem/reference/neigh_inclusion.md))

- direction:

  Whether `P[u, v] = 1` means that `u` is `dominated` by `v` (default)
  or that `u` `dominates` `v`

## Value

This function returns the number and proportion of comparable pairs, and
a table of the unordered pairs, where `u < v` means that `u` is
dominated by `v`.

## Details

Two nodes are comparable if one is dominated by the other. Centrality
indices resolve the incomparable pairs with a total ranking, which is
not required by the structure of the network.

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

dominance_pairs(neigh_inclusion(A))
#> $comparable
#> [1] 7
#> 
#> $incomparable
#> [1] 3
#> 
#> $prop_comparable
#> [1] 0.7
#> 
#> $pairs
#>    u v comparable  direction
#> 1  a b       TRUE      b < a
#> 2  a c      FALSE       <NA>
#> 3  a d       TRUE      d < a
#> 4  a e       TRUE      e < a
#> 5  b c       TRUE      b < c
#> 6  b d       TRUE equivalent
#> 7  b e      FALSE       <NA>
#> 8  c d       TRUE      d < c
#> 9  c e       TRUE      e < c
#> 10 d e      FALSE       <NA>
#> 
```
