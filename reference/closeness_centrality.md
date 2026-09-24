# Closeness centrality

Closeness centrality of Freeman (1978) and its harmonic version
(Marchiori and Latora, 2000; Rochat, 2009).

## Usage

``` r
closeness_centrality(
  A,
  digraph = TRUE,
  type = c("out", "in", "all"),
  weighted = FALSE,
  alpha = 1,
  harmonic = FALSE,
  normalized = FALSE
)
```

## Arguments

- A:

  A square matrix

- digraph:

  Whether the matrix is directed or undirected

- type:

  Whether to use the `out` (default), `in` or `all` distances. The `all`
  option uses the underlying graph

- weighted:

  Whether the matrix is weighted

- alpha:

  The tuning parameter of Opsahl et al. (2010) to transform weights into
  lengths

- harmonic:

  Whether to return the harmonic closeness

- normalized:

  If TRUE, Freeman's closeness is multiplied by the number of nodes
  reached, and the harmonic closeness is divided by (n-1)

## Value

This function returns the closeness centrality of the nodes.

## Details

Freeman's closeness is the inverse of the sum of the geodesic distances
from a node to the others. When the network is disconnected, the sum
only considers the nodes that can be reached, and the harmonic version
is recommended, as it adds the inverse of each distance (an unreachable
node adds zero).

For valued matrices, the tie weights are treated as strengths and
transformed into lengths as \\1 / w^{\alpha}\\ (Opsahl et al., 2010). If
`alpha = 0` the binary network is used.

## References

Freeman, L. C. (1978). Centrality in social networks conceptual
clarification. Social Networks, 1(3), 215–239.
[doi:10.1016/0378-8733(78)90021-7](https://doi.org/10.1016/0378-8733%2878%2990021-7)

Marchiori, M. and Latora, V. (2000). Harmony in the small-world. Physica
A, 285(3-4), 539–546.
[doi:10.1016/S0378-4371(00)00311-3](https://doi.org/10.1016/S0378-4371%2800%2900311-3)

Opsahl, T., Agneessens, F., and Skvoretz, J. (2010). Node centrality in
weighted networks: Generalizing degree and shortest paths. Social
Networks, 32(3), 245–251.
[doi:10.1016/j.socnet.2010.03.006](https://doi.org/10.1016/j.socnet.2010.03.006)

Rochat, Y. (2009). Closeness centrality extended to unconnected graphs:
The harmonic centrality index. ASNA.

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

closeness_centrality(A, digraph = FALSE)
#>         a         b         c         d         e 
#> 0.2000000 0.1250000 0.1666667 0.1250000 0.1111111 
closeness_centrality(A, digraph = FALSE, harmonic = TRUE)
#>        a        b        c        d        e 
#> 3.500000 2.333333 3.000000 2.333333 2.166667 
```
