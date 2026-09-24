# Alter homophily

E-I index and Yule's Q of each node, for categories that can overlap
(Everett and Borgatti, 2026).

## Usage

``` r
alter_homophily(
  A,
  B,
  method = c("ei", "yule"),
  similarity = c("product", "minimum", "cosine")
)
```

## Arguments

- A:

  A square matrix

- B:

  A matrix of the membership of the nodes (rows) in the categories
  (columns), or a vector with the category of each node

- method:

  The index: `ei` (default) or `yule`

- similarity:

  The similarity of the memberships: `product` (default), `minimum` or
  `cosine`

## Value

This function returns a vector with the index of each node.

## Details

The internal ties of a node \\i\\ are \\I = \sum_j A\_{ij} S\_{ij}\\,
where \\S\_{ij}\\ is the similarity of the memberships of \\i\\ and
\\j\\, and the external ties are \\E = D - I\\, where \\D\\ is the
degree. The E-I index is \\(E - I) / (E + I)\\ (Krackhardt and Stern,
1988): -1 when all the alters are in the categories of ego (homophily)
and +1 when none is (heterophily).

Yule's Q also uses the nodes that are not alters, to take into account
how many nodes of each category are available: \\a = I\\ and \\b = E\\
for the alters, and \\c\\ and \\d\\ are the same quantities for the
other nodes. Then \\Q = (ad - bc) / (ad + bc)\\, which is positive for
homophily, and zero when the ties do not depend on the categories.

The similarity of two memberships can be defined in three ways:

`similarity = "product"` (default), \\S\_{ij} = \sum_k B\_{ik}
B\_{jk}\\. It reduces to the usual indices when the categories are a
partition. If the categories are, for instance, the proportion of time
spent in each of several places, it is the probability that two nodes
are in the same place.

`similarity = "minimum"`, \\S\_{ij} = \sum_k \min(B\_{ik}, B\_{jk})\\,
the trait version (\\E_s-I_s\\ and \\Q_s\\). Two nodes with identical
memberships are fully similar even when they split their time among
categories, which fits categories that are traits such as skills or
interests.

`similarity = "cosine"`, the cosine of the memberships, which favours
two nodes that concentrate in the same categories, such as two
specialists with the same specialty.

The ties are binary and the loops are ignored. For a directed network
the alters are the out-neighbours. Nodes without alters, and Yule's Q
with \\ad + bc = 0\\, are `NA`.

## References

Everett, M. G. and Borgatti, S. P. (2026). Alter composition with
overlapping group memberships. Social Networks, 85, 80–88.
[doi:10.1016/j.socnet.2025.12.001](https://doi.org/10.1016/j.socnet.2025.12.001)

Krackhardt, D. and Stern, R. N. (1988). Informal networks and
organizational crises: An experimental simulation. Social Psychology
Quarterly, 51(2), 123–140.
[doi:10.2307/2786835](https://doi.org/10.2307/2786835)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0,
  1, 0, 1, 1,
  1, 1, 0, 0,
  0, 1, 0, 0
), byrow = TRUE, ncol = 4)
rownames(A) <- colnames(A) <- c("a", "b", "c", "d")
B <- matrix(c(
  10, 0,
  5, 5,
  0, 8,
  2, 6
), byrow = TRUE, ncol = 2)

alter_homophily(A, B)
#>   a   b   c   d 
#> 0.5 0.0 0.5 0.0 
alter_homophily(A, B, method = "yule", similarity = "minimum")
#>    a    b    c    d 
#>  0.0   NA -0.8  0.5 
```
