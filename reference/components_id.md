# Components

Components of a network: the groups of nodes that can reach each other.

## Usage

``` r
components_id(A, mode = c("weak", "strong"), bipartite = FALSE)
```

## Arguments

- A:

  A square matrix, or an incidence matrix if `bipartite = TRUE`

- mode:

  Whether the components are `weak` (default) or `strong`

- bipartite:

  Whether the matrix is an incidence matrix of a two-mode network

## Value

A vector with the component of each node, and the size of the
components.

## Details

In a `weak` component the nodes are connected when the direction of the
ties is ignored. In a `strong` component every node can reach every
other node following the direction of the ties, so a strong component is
contained in a weak one. For undirected networks both are the same.

For two-mode networks, the nodes of both sets are placed in the same
network before looking for the components, so a component contains the
nodes of the first mode and the ones of the second mode that they share.

## References

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r

A <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 0, 0,
  1, 1, 0, 0, 0,
  0, 0, 0, 0, 1,
  0, 0, 0, 1, 0
), byrow = TRUE, ncol = 5)
rownames(A) <- letters[1:ncol(A)]
colnames(A) <- rownames(A)
components_id(A)
#> $components
#> a b c d e 
#> 1 1 1 2 2 
#> 
#> $size
#> components
#> 1 2 
#> 3 2 
#> 

# In a chain of citations every node is in the same weak component,
# but each of them is its own strong component
B <- matrix(c(
  0, 1, 0, 0,
  0, 0, 1, 0,
  0, 0, 0, 1,
  0, 0, 0, 0
), byrow = TRUE, ncol = 4)
components_id(B)
#> $components
#> [1] 1 1 1 1
#> 
#> $size
#> components
#> 1 
#> 4 
#> 
components_id(B, mode = "strong")
#> $components
#> [1] 1 2 3 4
#> 
#> $size
#> components
#> 1 2 3 4 
#> 1 1 1 1 
#> 
```
