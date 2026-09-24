# Transform an edgelist to a matrix

Transform an edgelist to a matrix

## Usage

``` r
edgelist_to_matrix(
  E,
  digraph = TRUE,
  label = NULL,
  label2 = NULL,
  bipartite = FALSE,
  valued = FALSE,
  loops = FALSE,
  rule = c("weak", "strong")
)
```

## Arguments

- E:

  An edge list

- digraph:

  Whether the matrix is directed or not

- label:

  A vector with the names of the nodes, which gives their order in the
  matrix and adds the nodes without ties

- label2:

  A vector with the names of the nodes of the second mode, with the same
  role as `label`, when `bipartite = TRUE`

- bipartite:

  Whether the matrix is bipartite

- valued:

  Whether the third column of the edgelist has the value of the tie

- loops:

  Whether to keep the ties of a node with itself

- rule:

  For `digraph = FALSE`, whether an undirected tie is kept when it is
  listed in either order (`weak`, default) or only when it is listed in
  both orders (`strong`), as in `sna::symmetrize`

## Value

This function transform the edgelist into a matrix

## Details

With `digraph = FALSE` each undirected tie is placed in both cells of
the matrix, so the number of ties is the number of cells of one triangle
(or half the sum of a binary matrix). A tie listed in both orders is a
single tie.

The rows and the columns follow the order of `label`. The nodes that are
not in `label`, or all the nodes when it is not given, are added in
alphabetical order.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0, 0, 1, 0,
  1, 0, 1, 0, 0, 0, 0, 0, 0,
  1, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 1, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 1, 1, 0,
  0, 0, 0, 0, 0, 1, 0, 1, 0,
  1, 0, 0, 0, 0, 1, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0
), byrow = TRUE, ncol = 9)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)
E <- matrix_to_edgelist(A)
edgelist_to_matrix(E, label = c("i"), digraph = FALSE)
#>   i a b c d e f g h
#> i 0 0 0 0 0 0 0 0 0
#> a 0 0 1 1 0 0 0 0 1
#> b 0 1 0 1 0 0 0 0 0
#> c 0 1 1 0 0 0 0 0 0
#> d 0 0 0 0 0 1 1 0 0
#> e 0 0 0 0 1 0 0 0 0
#> f 0 0 0 0 1 0 0 1 1
#> g 0 0 0 0 0 0 1 0 1
#> h 0 1 0 0 0 0 1 1 0

# With a third column, the ties keep their value
V <- rbind(
  c("a", "b", 3),
  c("b", "c", 1),
  c("c", "a", 7)
)
edgelist_to_matrix(V, valued = TRUE)
#>   a b c
#> a 0 3 0
#> b 0 0 1
#> c 7 0 0
```
