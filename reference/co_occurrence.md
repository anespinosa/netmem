# Co‐occurrence

Co‐occurrence matrix based on overlap function

## Usage

``` r
co_occurrence(
  A,
  similarity = c("ochiai", "cosine"),
  occurrence = TRUE,
  projection = FALSE
)
```

## Arguments

- A:

  A matrix

- similarity:

  The similarities available are either `Ochiai` (default) or `cosine`.

- occurrence:

  Whether to treat the matrix as a two-mode structure (a.k.a.
  rectangular matrix, occurrence matrix, affiliation matrix, bipartite
  network)

- projection:

  Whether to apply a projection (inner product multiplication) to the
  matrix

## Value

This function returns the normalisation of a matrix into a symmetrical
co‐occurrence matrix

## References

Borgatti, S. P., Halgin, D. S., 2011. Analyzing affiliation networks.
In: J. Scott and P. J. Carrington (Eds.) The Sage handbook of social
network analysis (pp. 417-433), Sage.

Zhou, Q., & Leydesdorff, L. (2016). The normalization of occurrence and
Co-occurrence matrices in bibliometrics using Cosine similarities and
Ochiai coefficients. Journal of the Association for Information Science
and Technology, 67(11), 2805–2814.
[doi:10.1002/asi.23603](https://doi.org/10.1002/asi.23603)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(
  c(
    2, 0, 2,
    1, 1, 0,
    0, 3, 3,
    0, 2, 2,
    0, 0, 1
  ),
  nrow = 5, byrow = TRUE
)

co_occurrence(A)
#>           [,1]      [,2]      [,3]
#> [1,] 1.0000000 0.2357023 0.8164966
#> [2,] 0.2357023 1.0000000 1.8763884
#> [3,] 0.8164966 1.8763884 1.0000000
```
