# Indirect relations

Relations between the nodes that are derived from the ties of the
network, to be compared with
[`pos_dominance()`](https://anespinosa.github.io/netmem/reference/pos_dominance.md).

## Usage

``` r
indirect_rel(
  A,
  type = c("distance", "adjacency", "walks", "shared"),
  digraph = TRUE,
  alpha = 0.1
)
```

## Arguments

- A:

  A square matrix

- type:

  The relation: `adjacency`, `distance` (default), `walks` or `shared`

- digraph:

  Whether the matrix is directed or undirected

- alpha:

  The discount of the longer walks, for the `walks` relation

## Value

This function returns a square matrix of relations.

## Details

`adjacency`: the ties themselves.

`distance`: the length of the shortest path between the nodes.

`walks`: the number of walks of any length that join two nodes, where a
walk of length \\k\\ is discounted by \\\alpha^k\\, as in the Katz
centrality.

`shared`: the number of neighbours that two nodes have in common.

## References

Brandes, U. (2016). Network positions. Methodological Innovations, 9,
1–19.
[doi:10.1177/2059799116630650](https://doi.org/10.1177/2059799116630650)

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

indirect_rel(A, type = "distance", digraph = FALSE)
#>   a b c d e
#> a 0 1 1 1 2
#> b 1 0 2 2 3
#> c 1 2 0 2 1
#> d 1 2 2 0 3
#> e 2 3 1 3 0
indirect_rel(A, type = "walks", digraph = FALSE)
#>           a          b         c          d          e
#> a 0.0310352 0.10310352 0.1041450 0.10310352 0.01041450
#> b 0.1031035 0.01031035 0.0104145 0.01031035 0.00104145
#> c 0.1041450 0.01041450 0.0206207 0.01041450 0.10206207
#> d 0.1031035 0.01031035 0.0104145 0.01031035 0.00104145
#> e 0.0104145 0.00104145 0.1020621 0.00104145 0.01020621
```
