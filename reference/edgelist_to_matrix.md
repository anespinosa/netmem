# Transform an edgelist to a matrix

Transform an edgelist to a matrix

## Usage

``` r
edgelist_to_matrix(
  E,
  digraph = TRUE,
  label = NULL,
  label2 = NULL,
  bipartite = FALSE
)
```

## Arguments

- E:

  An edge list

- digraph:

  Whether the matrix is directed or not

- label:

  A vector with the names of the nodes

- label2:

  A vector with the names of a different set of nodes

- bipartite:

  Whether the matrix is bipartite

## Value

This function transform the edgelist into a matrix

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
#>   a b c d e f g h i
#> a 0 1 1 0 0 0 0 1 0
#> b 1 0 1 0 0 0 0 0 0
#> c 1 1 0 0 0 0 0 0 0
#> d 0 0 0 0 1 1 0 0 0
#> e 0 0 0 1 0 0 0 0 0
#> f 0 0 0 1 0 0 1 1 0
#> g 0 0 0 0 0 1 0 1 0
#> h 1 0 0 0 0 1 1 0 0
#> i 0 0 0 0 0 0 0 0 0
```
