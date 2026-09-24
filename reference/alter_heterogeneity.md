# Alter heterogeneity

Blau's heterogeneity of the alters of each node, for categories that can
overlap (Everett and Borgatti, 2026).

## Usage

``` r
alter_heterogeneity(A, B, normalized = FALSE)
```

## Arguments

- A:

  A matrix of the ties of the nodes (rows) with their alters (columns)

- B:

  A matrix of the membership of the alters (rows) in the categories
  (columns), or a vector with the category of each alter

- normalized:

  Whether to return the IQV

## Value

This function returns a vector with the heterogeneity of the alters of
each node.

## Details

The heterogeneity of a node is \\1 - \sum_k p_k^2\\, where \\p_k\\ is
the proportion of its alters in category \\k\\, taken from the rows of
[`alter_composition()`](https://anespinosa.github.io/netmem/reference/alter_composition.md)
(Blau, 1977). A node with a single alter has some heterogeneity when
that alter belongs to several categories. The IQV divides the index by
its maximum, \\1 - 1/K\\, where \\K\\ is the number of categories. The
nodes without alters are `NA`.

## References

Blau, P. M. (1977). Inequality and heterogeneity: A primitive theory of
social structure. Free Press.

Everett, M. G. and Borgatti, S. P. (2026). Alter composition with
overlapping group memberships. Social Networks, 85, 80–88.
[doi:10.1016/j.socnet.2025.12.001](https://doi.org/10.1016/j.socnet.2025.12.001)

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

alter_heterogeneity(A, B)
#>         a         b         c         d 
#> 0.3750000 0.4861111 0.3750000 0.5000000 
```
