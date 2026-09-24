# Centralization

Centralization of Freeman (1979): how much a network is dominated by its
most central node.

## Usage

``` r
centrality_centralization(
  A,
  measure = c("degree", "closeness", "betweenness", "eigenvector"),
  digraph = FALSE,
  type = c("out", "in", "all"),
  loops = FALSE
)
```

## Arguments

- A:

  A square matrix

- measure:

  The centrality to be used: `degree` (default), `closeness`,
  `betweenness` or `eigenvector`

- digraph:

  Whether the matrix is directed or undirected

- type:

  Type of degree or distances for directed networks

- loops:

  Whether to consider the loops of the matrix

## Value

This function returns the centralization of the network, the centrality
scores and the theoretical maximum.

## Details

The centralization is the sum of the differences between the highest
centrality and the centrality of every node, divided by the largest sum
that a network of the same order can have. For degree, closeness and
betweenness the maximum is given by the star network, and for the
eigenvector centrality by a network with a single tie, which gives \\n -
2\\.

The function is named `centrality_centralization()` so that it does not
mask `sna::centralization()`.

## References

Freeman, L. C. (1979). Centrality in social networks conceptual
clarification. Social Networks, 1(3), 215–239.
[doi:10.1016/0378-8733(78)90021-7](https://doi.org/10.1016/0378-8733%2878%2990021-7)

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

centrality_centralization(A, measure = "degree", digraph = FALSE)
#> $centralization
#> [1] 0.5833333
#> 
#> $scores
#> a b c d e 
#> 3 1 2 1 1 
#> 
#> $maximum
#> [1] 12
#> 
centrality_centralization(A, measure = "betweenness", digraph = FALSE)
#> $centralization
#> [1] 0.7083333
#> 
#> $scores
#> a b c d e 
#> 5 0 3 0 0 
#> 
#> $maximum
#> [1] 24
#> 
```
