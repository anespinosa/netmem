# Betweenness centrality

Betweenness centrality of Freeman (1977), computed with the algorithm of
Brandes (2001).

## Usage

``` r
betweenness_centrality(
  A,
  digraph = TRUE,
  weighted = FALSE,
  alpha = 1,
  normalized = FALSE
)
```

## Arguments

- A:

  A square matrix

- digraph:

  Whether the matrix is directed or undirected

- weighted:

  Whether the matrix is weighted

- alpha:

  The tuning parameter of Opsahl et al. (2010) to transform weights into
  lengths

- normalized:

  If TRUE, the result is divided by (n-1)(n-2) for directed networks and
  (n-1)(n-2)/2 for undirected networks

## Value

This function returns the betweenness centrality of the nodes.

## Details

The betweenness of a node is the sum, over all pairs of other nodes, of
the proportion of geodesics between the pair that pass through the node.
For undirected networks each pair is counted once.

For valued matrices, the tie weights are treated as strengths and
transformed into lengths as \\1 / w^{\alpha}\\ (Opsahl et al., 2010). If
`alpha = 0` the binary network is used.

## References

Brandes, U. (2001). A faster algorithm for betweenness centrality.
Journal of Mathematical Sociology, 25(2), 163–177.
[doi:10.1080/0022250X.2001.9990249](https://doi.org/10.1080/0022250X.2001.9990249)

Freeman, L. C. (1977). A set of measures of centrality based on
betweenness. Sociometry, 40(1), 35–41.
[doi:10.2307/3033543](https://doi.org/10.2307/3033543)

Opsahl, T., Agneessens, F., and Skvoretz, J. (2010). Node centrality in
weighted networks: Generalizing degree and shortest paths. Social
Networks, 32(3), 245–251.
[doi:10.1016/j.socnet.2010.03.006](https://doi.org/10.1016/j.socnet.2010.03.006)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 1, 0, 0, 0, 0, 0,
  1, 0, 1, 1, 1, 0, 0, 0, 0,
  1, 1, 0, 1, 0, 1, 0, 0, 0,
  1, 1, 1, 0, 1, 1, 0, 0, 0,
  0, 1, 0, 1, 0, 1, 1, 0, 0,
  0, 0, 1, 1, 1, 0, 1, 0, 0,
  0, 0, 0, 0, 1, 1, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 1, 0
), byrow = TRUE, ncol = 9)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

betweenness_centrality(A, digraph = FALSE)
#>         a         b         c         d         e         f         g         h 
#>  0.000000  1.583333  1.583333  3.166667  6.333333  6.333333 12.000000  7.000000 
#>         i 
#>  0.000000 
```
