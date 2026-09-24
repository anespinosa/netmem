# Mixing matrix

Create a mixing matrix from node attributes. The mixing matrix is a
two-dimensional matrix that cross-classifies the edges depending on the
values of their attributes. This matrix allowed identifying segregation
and homophily at the network level.

## Usage

``` r
mix_matrix(A, att = NULL, digraph = TRUE)
```

## Arguments

- A:

  A symmetric matrix object

- att:

  Categorical attribute of the nodes

- digraph:

  Whether the matrix is directed. A symmetric matrix is always treated
  as undirected

## Value

This function returns a mixing matrix

## Details

Values in the diagonal are the number of ties within groups, and
off-diagonal are the number of relations between groups. For directed
networks the entries are the arcs between the groups. For undirected
networks every edge is counted once, so the entries outside the diagonal
are half of the edges between the two groups.

## Author

Alejandro Espinosa-Rada

## Examples

``` r

n <- 100
A <- matrix(c(rbinom(n, 1, 0.5)),
  ncol = sqrt(n), nrow = sqrt(n), byrow = TRUE
)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]
att <- rbinom(sqrt(n), 3, 0.5)
mix_matrix(A, att = att)
#>     To
#> From 0 1 2 3
#>    0 7 3 2 3
#>    1 4 1 1 3
#>    2 4 3 1 1
#>    3 3 3 2 1
```
