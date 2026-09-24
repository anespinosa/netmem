# Geodesic distances

Matrix of geodesic distances and a summary of the distances of the
network.

## Usage

``` r
geo_distances(
  A,
  digraph = TRUE,
  type = c("out", "in", "all"),
  weighted = FALSE,
  alpha = 1
)

geo_summary(A, digraph = TRUE, weighted = FALSE, alpha = 1)
```

## Arguments

- A:

  A square matrix

- digraph:

  Whether the matrix is directed or undirected

- type:

  Whether to use the `out` (default), `in` or `all` distances

- weighted:

  Whether the matrix is weighted

- alpha:

  The tuning parameter of Opsahl et al. (2010) to transform weights into
  lengths

## Value

`geo_distances` returns a matrix of distances, and `geo_summary` the
diameter, the average distance and the proportion of reachable pairs.

## Details

`geo_distances` returns the length of the shortest path between every
pair of nodes, computed with the Floyd-Warshall algorithm in matrix
form. The distance is infinite when there is no path. For valued
matrices, the weights are treated as strengths and transformed into
lengths as \\1 / w^{\alpha}\\ (Opsahl et al., 2010).

`geo_summary` returns the diameter (the longest geodesic distance), the
average distance and the proportion of ordered pairs that can reach each
other. When the network is disconnected, both the diameter and the
average distance only consider the pairs that are connected by a path.

## References

Opsahl, T., Agneessens, F., and Skvoretz, J. (2010). Node centrality in
weighted networks: Generalizing degree and shortest paths. Social
Networks, 32(3), 245–251.
[doi:10.1016/j.socnet.2010.03.006](https://doi.org/10.1016/j.socnet.2010.03.006)

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  0, 0, 0, 1, 1, 0,
  0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0
), byrow = TRUE, nrow = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]

geo_distances(A)
#>     a   b   c   d   e   f
#> a   0   1   1   2   2   3
#> b Inf   0 Inf   1   1   2
#> c Inf Inf   0 Inf   1   2
#> d Inf Inf Inf   0 Inf Inf
#> e Inf Inf Inf Inf   0   1
#> f Inf Inf Inf Inf Inf   0
geo_summary(A)
#> $diameter
#> [1] 3
#> 
#> $average_distance
#> [1] 1.545455
#> 
#> $prop_reachable
#> [1] 0.3666667
#> 
```
