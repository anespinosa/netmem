# Mixing matrix

Create a mixing matrix from node attributes. The mixing matrix is a
two-dimensional matrix that cross-classifies the edges depending on the
values of their attributes. This matrix allowed identifying segregation
and homophily at the network level.

## Usage

``` r
mix_matrix(A, att = NULL)
```

## Arguments

- A:

  A symmetric matrix object

- att:

  Categorical attribute of the nodes

## Value

This function returns a mixing matrix.

## Details

Values in the diagonal are the number of ties within groups, and
off-diagonal are the number of relations between groups.

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
#> From 0 1 2
#>    0 0 1 3
#>    1 0 0 4
#>    2 1 3 9
```
