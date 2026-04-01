# Transitivity matrix

This function assigns a one in the elements of the matrix if a group of
actors are part of a transitivity structure (030T label considering the
MAN triad census)

## Usage

``` r
trans_matrix(A, loops = FALSE)
```

## Arguments

- A:

  A matrix

- loops:

  Whether to expect nonzero elements in the diagonal of the matrix

## Value

A vector assigning an id the components that each of the nodes of the
matrix belongs

## References

Davis, J.A. and Leinhardt, S. (1972). “The Structure of Positive
Interpersonal Relations in Small Groups.” In J. Berger (Ed.),
Sociological Theories in Progress, Vol. 2, 218-251. Boston: Houghton
Mifflin.

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(
  c(
    0, 1, 1, 0, 0, 0,
    0, 0, 1, 0, 0, 0,
    0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0,
    0, 0, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0
  ),
  byrow = TRUE, ncol = 6
)
rownames(A) <- letters[1:NROW(A)]
colnames(A) <- rownames(A)
trans_matrix(A, loops = TRUE)
#>   a b c d e f
#> a 1 1 1 0 0 0
#> b 1 1 1 0 0 0
#> c 1 1 1 1 1 0
#> d 0 0 1 1 1 0
#> e 0 0 1 1 1 0
#> f 0 0 0 0 0 0
```
