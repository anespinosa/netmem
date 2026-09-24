# Brokerage roles

Brokerage roles of Gould and Fernandez (1989), for categories that can
overlap (Everett and Borgatti, 2026).

## Usage

``` r
brokerage_roles(A, B)
```

## Arguments

- A:

  A square matrix

- B:

  A matrix of the membership of the nodes (rows) in the categories
  (columns), or a vector with the category of each node

## Value

This function returns a matrix with the score of each node (rows) in
each role, and the total.

## Details

A node \\b\\ brokers in a path \\a \to b \to c\\ when \\a\\ has no tie
to \\c\\. The role depends on the categories of the three nodes:
`coordinator` (all in the same category), `gatekeeper` (\\b\\ and \\c\\
in the same category, \\a\\ in another), `representative` (\\a\\ and
\\b\\ in the same category, \\c\\ in another), `consultant` (also called
itinerant; \\a\\ and \\c\\ in the same category, \\b\\ in another) and
`liaison` (the three in different categories).

With overlapping categories, each path counts partly for each role
(Everett and Borgatti, 2026: Eq. 5). The coordinator part is \\\sum_j
B\_{aj} B\_{bj} B\_{cj}\\, the joint membership of the three nodes in
each category; the gatekeeper part is \\\sum_j B\_{bj} B\_{cj} (1 -
B\_{aj})\\; the representative part \\\sum_j B\_{aj} B\_{bj} (1 -
B\_{cj})\\; the consultant part \\\sum_j B\_{aj} B\_{cj} (1 -
B\_{bj})\\; and the liaison part is the rest. When the categories are a
partition, each path counts for a single role, as in Gould and Fernandez
(1989). The parts of each path add up to one, so the total of each node
is the number of paths it brokers.

A symmetric matrix is treated as a directed network with ties in both
directions, so each path is counted from both ends.

## References

Everett, M. G. and Borgatti, S. P. (2026). Alter composition with
overlapping group memberships. Social Networks, 85, 80–88.
[doi:10.1016/j.socnet.2025.12.001](https://doi.org/10.1016/j.socnet.2025.12.001)

Gould, R. V. and Fernandez, R. M. (1989). Structures of mediation: A
formal approach to brokerage in transaction networks. Sociological
Methodology, 19, 89–126.
[doi:10.2307/270949](https://doi.org/10.2307/270949)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 0, 0,
  0, 0, 1, 1,
  0, 0, 0, 1,
  1, 0, 0, 0
), byrow = TRUE, ncol = 4)
rownames(A) <- colnames(A) <- c("a", "b", "c", "d")

brokerage_roles(A, c("x", "x", "y", "y"))
#>   coordinator gatekeeper representative consultant liaison total
#> a           0          1              0          0       0     1
#> b           0          0              2          0       0     2
#> c           0          0              0          0       0     0
#> d           0          0              1          1       0     2

B <- matrix(c(
  10, 0,
  5, 5,
  0, 8,
  2, 6
), byrow = TRUE, ncol = 2)
brokerage_roles(A, B)
#>   coordinator gatekeeper representative consultant liaison total
#> a       0.125      0.375          0.125      0.375       0     1
#> b       0.125      0.875          0.875      0.125       0     2
#> c       0.000      0.000          0.000      0.000       0     0
#> d       0.125      0.375          1.125      0.375       0     2
```
