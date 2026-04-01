# Meta matrix for multilevel networks

Meta matrix for multilevel networks

## Usage

``` r
meta_matrix(A1, B1, A2 = NULL, B2 = NULL, A3 = NULL, B3 = NULL)
```

## Arguments

- A1:

  The square matrix of the lowest level

- B1:

  The incidence matrix of the ties between the nodes of first level and
  the nodes of the second level

- A2:

  The square matrix of the second level

- B2:

  The incidence matrix of the ties between the nodes of the second level
  and the nodes of the third level

- A3:

  The square matrix of the third level

- B3:

  The incidence matrix of the ties between the nodes of the third level
  and the nodes of the first level

## Value

Return a meta matrix for multilevel networks

## References

Carley, K. M. (2002). Smart agents and organizations of the future. In:
Leah Lievrouw & Sonia Livingstone (Eds.), The Handbook of New Media (pp.
206-220). Thousand Oaks, CA, Sage.

Krackhardt, D., & Carley, K. M. (1998). PCANS model of structure in
organizations (pp. 113- 119). Pittsburgh, Pa, USA: Carnegie Mellon
University, Institute for Complex Engineered Systems.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A1 <- matrix(c(
  0, 1, 0, 0, 0,
  1, 0, 0, 1, 0,
  0, 0, 0, 1, 0,
  0, 1, 1, 0, 1,
  0, 0, 0, 1, 0
), byrow = TRUE, ncol = 5)

B1 <- matrix(c(
  1, 0, 0,
  1, 1, 0,
  0, 1, 0,
  0, 1, 0,
  0, 1, 1
), byrow = TRUE, ncol = 3)

A2 <- matrix(c(
  0, 1, 1,
  1, 0, 0,
  1, 0, 0
), byrow = TRUE, nrow = 3)

B2 <- matrix(c(
  1, 1, 0, 0,
  0, 0, 1, 0,
  0, 0, 1, 1
), byrow = TRUE, ncol = 4)

A3 <- matrix(c(
  0, 1, 1, 1,
  1, 0, 0, 0,
  1, 0, 0, 1,
  1, 0, 1, 0
), byrow = TRUE, ncol = 4)

B3 <- matrix(c(
  1, 0, 0, 0, 0,
  0, 1, 0, 1, 0,
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0
), byrow = TRUE, ncol = 5)

rownames(A1) <- letters[1:nrow(A1)]
colnames(A1) <- rownames(A1)
rownames(A2) <- letters[nrow(A1) + 1:nrow(A2)]
colnames(A2) <- rownames(A2)
rownames(B1) <- rownames(A1)
colnames(B1) <- colnames(A2)
rownames(A3) <- letters[nrow(A1) + nrow(A2) + 1:nrow(A3)]
colnames(A3) <- rownames(A3)
rownames(B2) <- rownames(A2)
colnames(B2) <- colnames(A3)
rownames(B3) <- rownames(A3)
colnames(B3) <- rownames(A1)
meta_matrix(A1, B1, A2, B2, A3, B3)
#>   a b c d e f g h i j k l
#> a 0 1 0 0 0 1 0 0 1 0 0 0
#> b 1 0 0 1 0 1 1 0 0 1 0 0
#> c 0 0 0 1 0 0 1 0 0 0 0 0
#> d 0 1 1 0 1 0 1 0 0 1 0 0
#> e 0 0 0 1 0 0 1 1 0 0 0 0
#> f 1 1 0 0 0 0 1 1 0 0 0 0
#> g 0 1 1 1 1 1 0 0 0 0 0 0
#> h 0 0 0 0 1 1 0 0 0 0 0 0
#> i 0 0 0 0 0 0 0 0 0 1 1 1
#> j 0 0 0 0 0 0 0 0 1 0 0 0
#> k 0 0 0 0 0 0 0 0 1 0 0 1
#> l 0 0 0 0 0 0 0 0 1 0 1 0
```
