# Generalized degree

Generalized degree centrality for one-mode and bipartite networks

## Usage

``` r
gen_degree(
  A,
  weighted = FALSE,
  type = "out",
  normalized = FALSE,
  loops = TRUE,
  digraph = TRUE,
  alpha = 0.5,
  bipartite = FALSE
)
```

## Arguments

- A:

  A matrix object

- weighted:

  Whether the matrix is weighted or not

- type:

  Character string, “out” (outdegree), “in” (indegree) and “all”
  (degree)

- normalized:

  Whether normalize the measure for the one-mode network (Freeman, 1978)
  or a bipartite network (Borgatti and Everett, 1997)

- loops:

  Whether the diagonal of the matrix is considered or not

- digraph:

  Whether the matrix is directed or undirected

- alpha:

  Sets the alpha parameter in the generalised measures from Opsahl et
  al. (2010)

- bipartite:

  Whether the matrix is bipartite or not.

## Value

This function returns term 1, 2 and 3, the normalization and the maximum
value of the specification of Everett and Borgatti (2020), and the
constraint of Burt (1992)

## References

Borgatti, S. P., and Everett, M. G. (1997). Network analysis of 2-mode
data. Social Networks, 19(3), 243–269.

Freeman, L. C. (1978). Centrality in social networks conceptual
clarification. Social Networks, 1(3), 215–239.

Opsahl, T., Agneessens, F., and Skvoretz, J. (2010). Node centrality in
weighted networks: Generalizing degree and shortest paths. Social
Networks, 32(3), 245–251.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A3 <- matrix(c(
  0, 4, 4, 0, 0, 0,
  4, 0, 2, 1, 1, 0,
  4, 2, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 7,
  0, 0, 0, 0, 7, 0
), byrow = TRUE, ncol = 6)

gen_degree(A3, digraph = FALSE, weighted = TRUE)
#> [1] 4.000000 5.656854 3.464102 1.000000 4.000000 2.645751
```
