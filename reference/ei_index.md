# Krackhardt and Stern's E-I index

This index was proposed by Krackhardt and Stern (1988) to distinguish
between the relative prevalence of between and within-group ties. This
measure can be interpreted as homophily at the network level.

## Usage

``` r
ei_index(A, mixed = TRUE, att = NULL)
```

## Arguments

- A:

  A symmetric matrix object, or a mixing matrix if no attribute is given

- mixed:

  Whether the matrix provided is already a mixing matrix. It is only
  used when no attribute is given

- att:

  Categorical attribute of the nodes. When it is given, the mixing
  matrix is computed from `A`

## Value

Numerical value of the E-I index.

## Examples

``` r

set.seed(18051889)
n <- 100
A <- matrix(c(rbinom(n, 1, 0.5)),
  ncol = sqrt(n), nrow = sqrt(n), byrow = TRUE
)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]

att <- rbinom(sqrt(n), 3, 0.5)
ei_index(A, att = att)
#> [1] 0.2093023

# All the ties are within the groups, so the index is -1
B <- matrix(0, 6, 6)
B[1:3, 1:3] <- 1
B[4:6, 4:6] <- 1
diag(B) <- 0
rownames(B) <- letters[1:6]
colnames(B) <- rownames(B)
ei_index(B, att = c(1, 1, 1, 2, 2, 2))
#> [1] -1
```
