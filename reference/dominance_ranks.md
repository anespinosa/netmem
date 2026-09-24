# Rank intervals

The ranks that a node can take in the rankings that are consistent with
a dominance relation (Schoch and Brandes, 2016).

## Usage

``` r
dominance_ranks(P, direction = c("dominated", "dominates"))
```

## Arguments

- P:

  A binary dominance matrix, such as the output of
  [`neigh_inclusion()`](https://anespinosa.github.io/netmem/reference/neigh_inclusion.md)

- direction:

  Whether `P[u, v] = 1` means that `u` is `dominated` by `v` (default)
  or that `u` `dominates` `v`

## Value

This function returns the minimum and the maximum rank of every node,
and the width of the interval.

## Details

A dominance relation only orders some pairs of nodes. Any centrality
index that preserves it gives a complete ranking, but different indices
give different rankings. The interval of a node contains every rank it
can take in such a ranking: it cannot be ranked below the nodes it
dominates, nor above the nodes that dominate it. A node with a wide
interval is one whose position depends on the index that is chosen, and
a node with an interval of a single value has the same rank under every
index that preserves the relation.

The rank one is the lowest.

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

dominance_ranks(neigh_inclusion(A))
#>   node min_rank max_rank width
#> 1    a        4        5     1
#> 2    b        1        2     1
#> 3    c        4        5     1
#> 4    d        1        2     1
#> 5    e        1        3     2
```
