# Degree centrality for multilevel networks

Degree centrality for multilevel networks

## Usage

``` r
multilevel_degree(
  A1,
  B1,
  A2 = NULL,
  B2 = NULL,
  A3 = NULL,
  B3 = NULL,
  complete = FALSE,
  digraphA1 = FALSE,
  digraphA2 = FALSE,
  digraphA3 = FALSE,
  typeA1 = "out",
  typeA2 = "out",
  typeA3 = "out",
  loopsA1 = FALSE,
  loopsA2 = FALSE,
  loopsA3 = FALSE,
  normalized = FALSE,
  weightedA1 = FALSE,
  weightedA2 = FALSE,
  weightedA3 = FALSE,
  alphaA1 = 0.5,
  alphaA2 = 0.5,
  alphaA3 = 0.5
)
```

## Arguments

- A1:

  The square matrix of the lowest level

- B1:

  The incidence matrix of the ties between the nodes of first level and
  the nodes of the second level

- A2:

  The square matrix of the second level

- B2:

  The incidence matrix of the ties between the nodes of the second level
  and the nodes of the third level

- A3:

  The square matrix of the third level

- B3:

  The incidence matrix of the ties between the nodes of the third level
  and the nodes of the first level

- complete:

  Add the degree of bipartite and tripartite networks for B1, B2 and/or
  B3, and the low_multilevel (i.e. A1+B1+B2+B3), meso_multilevel (i.e.
  B1+A2+B2+B3) and high_multilevel (i.e. B1+B2+A3+B3) degree

- digraphA1:

  Whether A1 is a directed network

- digraphA2:

  Whether A2 is a directed network

- digraphA3:

  Whether A3 is a directed network

- typeA1:

  Type of degree of the network for A1, "out" for out-degree, "in" for
  in-degree or "all" for the sum of the two

- typeA2:

  Type of degree of the network for A2, "out" for out-degree, "in" for
  in-degree or "all" for the sum of the two

- typeA3:

  Type of degree of the network for A3, "out" for out-degree, "in" for
  in-degree or "all" for the sum of the two

- loopsA1:

  Whether the loops of the edges are considered in matrix A1

- loopsA2:

  Whether the loops of the edges are considered in matrix A2

- loopsA3:

  Whether the loops of the edges are considered in matrix A3

- normalized:

  If TRUE then the result is divided by (n-1)+k+m for the first level,
  (m-1)+n+k for the second level, and (k-1)+m+n according to
  Espinosa-Rada et al. (2021)

- weightedA1:

  Whether A1 is weighted

- weightedA2:

  Whether A2 is weighted

- weightedA3:

  Whether A3 is weighted

- alphaA1:

  The alpha parameter of A1 according to Opsahl et al (2010) for
  weighted networks. The value 0.5 is given by default.

- alphaA2:

  The alpha parameter of A2 according to Opsahl et al (2010) for
  weighted networks. The value 0.5 is given by default.

- alphaA3:

  The alpha parameter of A3 according to Opsahl et al (2010) for
  weighted networks. The value 0.5 is given by default.

## Value

Return a data.frame of multilevel degree

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
A1 <- matrix(c(
  0, 1, 0, 0, 0,
  1, 0, 0, 1, 0,
  0, 0, 0, 1, 0,
  0, 1, 1, 0, 1,
  0, 0, 0, 1, 0
), byrow = TRUE, ncol = 5)

B1 <- matrix(c(
  1, 0, 0,
  1, 1, 0,
  0, 1, 0,
  0, 1, 0,
  0, 1, 1
), byrow = TRUE, ncol = 3)

A2 <- matrix(c(
  0, 1, 1,
  1, 0, 0,
  1, 0, 0
), byrow = TRUE, nrow = 3)

B2 <- matrix(c(
  1, 1, 0, 0,
  0, 0, 1, 0,
  0, 0, 1, 1
), byrow = TRUE, ncol = 4)

A3 <- matrix(c(
  0, 1, 1, 1,
  1, 0, 0, 0,
  1, 0, 0, 1,
  1, 0, 1, 0
), byrow = TRUE, ncol = 4)

B3 <- matrix(c(
  1, 0, 0, 0, 0,
  0, 1, 0, 1, 0,
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0
), byrow = TRUE, ncol = 5)

multilevel_degree(A1, B1, A2, B2, A3, B3)
#>    multilevel
#> n1          3
#> n2          5
#> n3          2
#> n4          5
#> n5          3
#> m1          6
#> m2          6
#> m3          4
#> k1          5
#> k2          4
#> k3          4
#> k4          3
# \donttest{
multilevel_degree(A1, B1, A2, B2, A3, B3, normalized = TRUE, complete = TRUE)
#>    multilevel bipartiteB1 bipartiteB2 bipartiteB3 tripartiteB1B2 tripartiteB1B3
#> n1      0.273       0.333          NA        0.25          0.333          0.583
#> n2      0.455       0.667          NA        0.25          0.667          0.917
#> n3      0.182       0.333          NA        0.00          0.333          0.333
#> n4      0.455       0.333          NA        0.25          0.333          0.583
#> n5      0.273       0.667          NA        0.00          0.667          0.667
#> m1      0.545       0.400       0.500          NA          0.900          0.400
#> m2      0.545       0.800       0.250          NA          1.050          0.800
#> m3      0.364       0.200       0.500          NA          0.700          0.200
#> k1      0.455          NA       0.333        0.20          0.333          0.200
#> k2      0.364          NA       0.333        0.40          0.333          0.400
#> k3      0.364          NA       0.667        0.00          0.667          0.000
#> k4      0.273          NA       0.333        0.00          0.333          0.000
#>    tripartiteB2B3 tripartiteB1B2B3 low_multilevel meso_multilevel
#> n1          0.250            0.583          0.273           0.583
#> n2          0.250            0.917          0.455           0.917
#> n3          0.000            0.333          0.182           0.333
#> n4          0.250            0.583          0.455           0.583
#> n5          0.000            0.667          0.273           0.667
#> m1          0.500            0.900          0.900           0.545
#> m2          0.250            1.050          1.050           0.545
#> m3          0.500            0.700          0.700           0.364
#> k1          0.533            0.533          0.533           0.533
#> k2          0.733            0.733          0.733           0.733
#> k3          0.667            0.667          0.667           0.667
#> k4          0.333            0.333          0.333           0.333
#>    high_multilevel
#> n1           0.583
#> n2           0.917
#> n3           0.333
#> n4           0.583
#> n5           0.667
#> m1           0.900
#> m2           1.050
#> m3           0.700
#> k1           0.455
#> k2           0.364
#> k3           0.364
#> k4           0.273
# }
```
