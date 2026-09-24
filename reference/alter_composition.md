# Alter composition

Number of alters of each node in each category, for categories that can
overlap (Everett and Borgatti, 2026).

## Usage

``` r
alter_composition(A, B, proportion = FALSE)
```

## Arguments

- A:

  A matrix of the ties of the nodes (rows) with their alters (columns)

- B:

  A matrix of the membership of the alters (rows) in the categories
  (columns), or a vector with the category of each alter

- proportion:

  Whether to divide each row by its sum, giving the proportion of the
  alters in each category

## Value

This function returns a matrix of the nodes (rows) and the categories
(columns).

## Details

The memberships `B` are made row-stochastic, so that each row gives the
proportion of a node that belongs to each category (for instance, the
proportion of time spent on each project). The alter composition is the
product \\AB\\: for a partition, it counts the alters of each node in
each category; with overlapping categories, it gives the extent to which
the alters of each node belong to each category. The rows of \\AB\\ add
up to the degree of each node. `A` need not be square: its columns
should be the rows of `B`, as in a matrix of respondents and the alters
they named.

## References

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

# A partition
alter_composition(A, c("x", "x", "y", "y"))
#>   x y
#> a 1 1
#> b 1 2
#> c 2 0
#> d 1 0

# Overlapping categories: hours spent on two projects
B <- matrix(c(
  10, 0,
  5, 5,
  0, 8,
  2, 6
), byrow = TRUE, ncol = 2)
alter_composition(A, B)
#>     G1   G2
#> a 0.50 1.50
#> b 1.25 1.75
#> c 1.50 0.50
#> d 0.50 0.50
```
