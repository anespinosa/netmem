# Partition of centrality by category

Contribution of the nodes of each category to the centrality of every
node (Everett and Borgatti, 2012, 2026).

## Usage

``` r
partition_centrality(
  A,
  B,
  measure = c("betweenness", "degree"),
  digraph = TRUE,
  type = c("out", "in"),
  weighted = FALSE,
  alpha = 1
)
```

## Arguments

- A:

  A square matrix

- B:

  A matrix of the membership of the nodes (rows) in the categories
  (columns), or a vector with the category of each node

- measure:

  The centrality: `betweenness` (default) or `degree`

- digraph:

  Whether the matrix is directed or undirected

- type:

  For the degree of a directed network, `out` (default) or `in`

- weighted:

  Whether the betweenness uses the values of the ties, as in
  [`betweenness_centrality()`](https://anespinosa.github.io/netmem/reference/betweenness_centrality.md)

- alpha:

  The alpha parameter of Opsahl et al. (2010) for the weighted
  betweenness

## Value

This function returns a matrix with the part of the centrality of each
node (rows) contributed by each category (columns).

## Details

The betweenness of a node \\v\\ adds up the dependency of every source
\\s\\ on \\v\\, \\\delta_s(v)\\, the extent to which \\s\\ needs \\v\\
to reach the other nodes through shortest paths (Brandes, 2001).
Grouping the sources by their category, \\D^T B\\, splits the
betweenness of each node into the parts contributed by each category,
and with overlapping categories each source contributes in proportion to
its memberships. The rows add up to the betweenness of
[`betweenness_centrality()`](https://anespinosa.github.io/netmem/reference/betweenness_centrality.md).

The degree is split with the alter composition, \\AB\\ for the
out-degree and \\A^T B\\ for the in-degree.

## References

Brandes, U. (2001). A faster algorithm for betweenness centrality.
Journal of Mathematical Sociology, 25(2), 163–177.
[doi:10.1080/0022250X.2001.9990249](https://doi.org/10.1080/0022250X.2001.9990249)

Everett, M. G. and Borgatti, S. P. (2012). Categorical attribute based
centrality: E–I and G–F centrality. Social Networks, 34(4), 562–569.
[doi:10.1016/j.socnet.2012.06.002](https://doi.org/10.1016/j.socnet.2012.06.002)

Everett, M. G. and Borgatti, S. P. (2026). Alter composition with
overlapping group memberships. Social Networks, 85, 80–88.
[doi:10.1016/j.socnet.2025.12.001](https://doi.org/10.1016/j.socnet.2025.12.001)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
data(campnet)
# Betweenness of the Camp 92 network split by the gender of the sources
partition_centrality(campnet$network, campnet$attributes$gender)
#>                 1         2
#> HOLLY   24.333333 54.000000
#> BRAZEY   0.000000  0.000000
#> CAROL    1.333333  0.000000
#> PAM      8.500000 24.000000
#> PAT     25.000000 14.500000
#> JENNIE   6.333333  0.000000
#> PAULINE  7.000000  5.500000
#> ANN      0.500000  0.000000
#> MICHAEL  9.000000 49.833333
#> BILL     0.000000  0.000000
#> LEE      0.000000  5.000000
#> DON     14.000000  2.333333
#> JOHN     0.000000  0.000000
#> HARRY    0.000000  2.333333
#> GERY    10.000000 44.666667
#> STEVE    6.000000 10.833333
#> BERT     6.000000  7.666667
#> RUSS    11.000000 36.333333
```
