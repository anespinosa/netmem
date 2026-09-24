# Structural holes

Effective size, efficiency and constraint of Burt (1992) for every node
of a valued or directed network, with the option of treating the alters
of the same category as redundant (Everett and Borgatti, 2026).

## Usage

``` r
structural_holes(A, B = NULL, beta = 1, ego_network = TRUE)
```

## Arguments

- A:

  A square matrix, binary or valued

- B:

  An optional matrix of the membership of the nodes (rows) in the
  categories (columns), or a vector with the category of each node

- beta:

  The strength of the tie added between two alters of the same category

- ego_network:

  Whether the measures are computed within the ego network of each node
  (TRUE) or within the whole network

## Value

This function returns a data frame with the number of alters, the
effective size, the efficiency and the constraint of each node.

## Details

Burt (1992) measures the ties of a node \\i\\ with its alters \\j\\
through the proportion of the ties of \\i\\ that go to \\j\\, \\p\_{ij}
= (a\_{ij} + a\_{ji}) / \sum_k (a\_{ik} + a\_{ki})\\, so a directed tie
counts in both directions. The effective size is \\\sum_j (1 - \sum_q
p\_{iq} m\_{jq})\\, where \\m\_{jq}\\ is the tie of \\j\\ with \\q\\
divided by the strongest tie of \\j\\; the efficiency is the effective
size divided by the number of alters; and the constraint is \\\sum_j
(p\_{ij} + \sum_q p\_{iq} p\_{qj})^2\\.

With `ego_network = TRUE` (default, as in UCINET) the proportions are
computed within the ego network of each node, and with
`ego_network = FALSE` within the whole network, as in
[`igraph::constraint()`](https://r.igraph.org/reference/constraint.html).
For binary undirected networks and ego networks, the effective size is
the one of
[`redundancy()`](https://anespinosa.github.io/netmem/reference/redundancy.md)
and the constraint the one of
[`eb_constraint()`](https://anespinosa.github.io/netmem/reference/eb_constraint.md).

When `B` is given, two alters of the same category are treated as partly
redundant even when they are not tied: each missing tie between two
alters is given the value \\\beta \sum_k B\_{xk} B\_{yk}\\, the product
of their memberships, before the measures are computed (Everett and
Borgatti, 2026: Eq. 6). With a partition and `beta = 1` two alters of
the same category count as tied; `beta = 0` gives the original measures,
and the values in between set how much the category matters.

## References

Burt, R.S., 1992. Structural Holes: the Social Structure of Competition.
Harvard University Press, Cambridge.

Everett, M. G. and Borgatti, S. P. (2026). Alter composition with
overlapping group memberships. Social Networks, 85, 80–88.
[doi:10.1016/j.socnet.2025.12.001](https://doi.org/10.1016/j.socnet.2025.12.001)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
data(campnet)
structural_holes(campnet$network)
#>         alters effective_size efficiency constraint
#> HOLLY        5       3.857143  0.7714286  0.4363719
#> BRAZEY       3       1.000000  0.3333333  1.0238889
#> CAROL        3       2.000000  0.6666667  0.8044444
#> PAM          5       3.875000  0.7750000  0.5163194
#> PAT          4       3.571429  0.8928571  0.3900227
#> JENNIE       3       2.333333  0.7777778  0.6111111
#> PAULINE      5       3.857143  0.7714286  0.5365079
#> ANN          3       1.600000  0.5333333  0.9333333
#> MICHAEL      5       3.071429  0.6142857  0.6010603
#> BILL         3       1.000000  0.3333333  1.0800000
#> LEE          3       1.666667  0.5555556  0.8227778
#> DON          4       2.142857  0.5357143  0.7018141
#> JOHN         3       2.333333  0.7777778  0.7283951
#> HARRY        4       1.750000  0.4375000  0.7903321
#> GERY         4       2.900000  0.7250000  0.6491111
#> STEVE        5       3.062500  0.6125000  0.6148247
#> BERT         4       2.214286  0.5535714  0.7183263
#> RUSS         4       2.785714  0.6964286  0.6275510

# Alters of the same gender are partly redundant
structural_holes(campnet$network, B = campnet$attributes$gender, beta = 0.5)
#>         alters effective_size efficiency constraint
#> HOLLY        5       3.500000  0.7000000  0.5185714
#> BRAZEY       3       1.000000  0.3333333  1.0238889
#> CAROL        3       1.500000  0.5000000  0.9023480
#> PAM          5       2.000000  0.4000000  0.6359449
#> PAT          4       2.285714  0.5714286  0.6843537
#> JENNIE       3       1.666667  0.5555556  0.8227778
#> PAULINE      5       3.142857  0.6285714  0.5869581
#> ANN          3       1.300000  0.4333333  0.9467222
#> MICHAEL      5       2.047619  0.4095238  0.6829291
#> BILL         3       1.000000  0.3333333  1.0800000
#> LEE          3       1.666667  0.5555556  0.8227778
#> DON          4       2.071429  0.5178571  0.7123272
#> JOHN         3       2.333333  0.7777778  0.7283951
#> HARRY        4       1.666667  0.4166667  0.8059043
#> GERY         4       1.483333  0.3708333  0.7961985
#> STEVE        5       2.437500  0.4875000  0.6535387
#> BERT         4       1.928571  0.4821429  0.7505675
#> RUSS         4       1.797619  0.4494048  0.7328385
```
