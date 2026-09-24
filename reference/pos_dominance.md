# Positional dominance on indirect relations

Dominance between the rows of a matrix of relations, which do not need
to be the ties themselves (Brandes, 2016; Schoch and Brandes, 2016).

## Usage

``` r
pos_dominance(
  R,
  map = FALSE,
  benefit = TRUE,
  direction = c("dominated", "dominates")
)
```

## Arguments

- R:

  A square matrix of relations, such as the output of
  [`indirect_rel()`](https://anespinosa.github.io/netmem/reference/indirect_rel.md)

- map:

  Whether the values are sorted before being compared (total
  homogeneity)

- benefit:

  Whether a larger value is better

- direction:

  Whether `P[u, v] = 1` means that `u` is `dominated` by `v` (default)
  or that `u` `dominates` `v`

## Value

This function returns a binary matrix `P` of the positional dominance,
where `P[u, v] = 1` when every indirect relation of `u` is at most the
one of `v`, in the `direction` asked for.

## Details

Neighbourhood inclusion compares the ties of the nodes. The same
comparison can be made on any relation derived from the network, such as
the distances between the nodes or the number of walks that join them,
which is what makes different centrality indices comparable.

Under total heterogeneity (`map = FALSE`) the values are compared one by
one: \\i\\ is dominated by \\j\\ when its relation with every other node
is at most as large. Under total homogeneity (`map = TRUE`) the values
are sorted before being compared, so it does not matter with whom the
relation is held, only how large the values are.

With `benefit = FALSE` a smaller value is better, which is the case of
distances.

## References

Brandes, U. (2016). Network positions. Methodological Innovations, 9,
1–19.
[doi:10.1177/2059799116630650](https://doi.org/10.1177/2059799116630650)

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

# Distances: the closer to the others, the better
D <- indirect_rel(A, type = "distance", digraph = FALSE)
pos_dominance(D, benefit = FALSE)
#>   a b c d e
#> a 0 0 0 0 0
#> b 1 0 1 1 0
#> c 0 0 0 0 0
#> d 1 1 1 0 0
#> e 1 0 1 0 0
pos_dominance(D, benefit = FALSE, map = TRUE)
#>   a b c d e
#> a 0 0 0 0 0
#> b 1 0 1 1 0
#> c 1 0 0 0 0
#> d 1 1 1 0 0
#> e 1 1 1 1 0
```
