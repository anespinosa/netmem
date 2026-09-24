# Degree centrality for multilevel networks

Degree of the nodes of a network of two or three levels, counting the
ties within each level and the ties between the levels in different
combinations.

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

  Whether to return every column described in the details, instead of
  only the `multilevel` degree

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

  Whether to divide each degree by the largest value it could take, as
  described in the details (Espinosa-Rada et al., 2021)

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

A data frame with one row for each node of every level, and the
`multilevel` degree, or every column described in the details when
`complete = TRUE`

## Details

The levels are placed in a single meta-matrix. Level one has `n` nodes
and the ties `A1`, level two has `m` nodes and the ties `A2`, and level
three has `k` nodes and the ties `A3`. The incidence matrices join the
levels: `B1` the first with the second (`n` by `m`), `B2` the second
with the third (`m` by `k`), and `B3` the third with the first (`k` by
`n`). A level that is not given has no ties within it.

Each column of the result emphasises a different section of the
meta-matrix:

`multilevel`: every node counts the ties within its own level and its
ties with the other levels, i.e. `A1 + B1 + B3` for the first level,
`B1 + A2 + B2` for the second, and `B2 + A3 + B3` for the third.

`bipartiteB1`, `bipartiteB2` and `bipartiteB3`: the degree in each
incidence matrix, i.e. only the ties between two levels.

`tripartiteB1B2`, `tripartiteB1B3`, `tripartiteB2B3` and
`tripartiteB1B2B3`: the degree in the union of incidence matrices, i.e.
only the ties between levels.

`low_multilevel` (`A1 + B1 + B2 + B3`), `meso_multilevel`
(`B1 + A2 + B2 + B3`) and `high_multilevel` (`B1 + B2 + A3 + B3`): the
ties within a single level, together with all the ties between levels.
For the nodes of the emphasised level they are the same as `multilevel`:
the first level in `low_multilevel`, the second in `meso_multilevel` and
the third in `high_multilevel`.

The rows are named `n1, n2, ...` for the first level, `m1, m2, ...` for
the second and `k1, k2, ...` for the third. Without `complete = TRUE`,
only the `multilevel` column is returned.

With `normalized = TRUE`, each degree is divided by the largest value it
could take: the other nodes of the same level plus the nodes of the
levels it is tied to. For the `multilevel` column this is `(n - 1) + m`
for the first level (`(n - 1) + m + k` when `B3` is given),
`(m - 1) + n + k` for the second level (`(m - 1) + n` with two levels),
and `(k - 1) + m` for the third level (`(k - 1) + m + n` when `B3` is
given). The bipartite degrees are divided by the number of nodes of the
other level (Borgatti and Everett, 1997). The normalized values are only
defined for binary matrices. All the values are rounded to three
decimals.

The ties within each level can be directed (`digraphA1`, `typeA1`, ...)
and weighted (`weightedA1`, `alphaA1`, ...), in which case the degree of
Opsahl et al. (2010) is used. The ties between levels are undirected.

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
