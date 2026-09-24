# Maximal cliques

Maximal complete subgraphs of an undirected network, found with the
algorithm of Bron and Kerbosch (1973).

## Usage

``` r
clique_max(A, min = 2, max = NULL)
```

## Arguments

- A:

  A symmetric matrix object

- min:

  Minimum size of the cliques returned

- max:

  Maximum size of the cliques returned. If NULL, there is no limit

## Value

This function returns a list with the names of the nodes of each maximal
clique.

## Details

A clique is a set of nodes that are all adjacent to each other, and it
is maximal when no other node can be added to it. Unlike
[`clique_table()`](https://anespinosa.github.io/netmem/reference/clique_table.md),
which returns the triangles of the network, this function returns
cliques of any size.

## References

Bron, C. and Kerbosch, J. (1973). Algorithm 457: Finding all cliques of
an undirected graph. Communications of the ACM, 16(9), 575–577.
[doi:10.1145/362342.362367](https://doi.org/10.1145/362342.362367)

Luce, R. D. and Perry, A. D. (1949). A method of matrix analysis of
group structure. Psychometrika, 14(2), 95–116.
[doi:10.1007/BF02289146](https://doi.org/10.1007/BF02289146)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 1, 0, 0,
  1, 1, 0, 1, 0, 0,
  0, 1, 1, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  0, 0, 0, 1, 1, 0
), byrow = TRUE, ncol = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

clique_max(A)
#> [[1]]
#> [1] "a" "b" "c"
#> 
#> [[2]]
#> [1] "b" "c" "d"
#> 
#> [[3]]
#> [1] "d" "e" "f"
#> 
```
