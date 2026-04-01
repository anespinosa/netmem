# Spatial autocorrelation

This function calculate some spatial autocorrelations for a sample of
networks at different orders (distances).

## Usage

``` r
spatial_cor(
  A,
  V,
  measures = c("covariance", "correlation", "moran", "geary"),
  mean = TRUE,
  diag = FALSE,
  distance1 = TRUE,
  rowstand = FALSE,
  scale = FALSE
)
```

## Arguments

- A:

  A symmetric matrix

- V:

  A vector

- measures:

  Whether to use the Covariance `covariance` (default), Correlation
  `correlation`, Moran I `moran` or Geary's C `geary`

- mean:

  Whether to use the mean of the vector for the measures

- diag:

  Whether to consider the diagonal of the matrix for the measures

- distance1:

  Whether to return only the spatial autocorrelation considering the
  actor at distance 1

- rowstand:

  Whether to use the row-standardization to estimate Moran I (Anselin,
  1995)

- scale:

  Whether to scale Moran I (Anselin, 1995)

## Value

This function return the global spatial autocorrelation. Multiple orders
can also be computed.

## References

Anselin, L. (1995). Local indicators of spatial association—LISA.
Geographical analysis, 27(2), 93-115.

Geary, R.C. (1954). “The Contiguity Ratio and Statistical Mapping.” The
Incorporated Statistician, 5: 115-145.

Moran, P.A.P. (1950). “Notes on Continuous Stochastic Phenomena.”
Biometrika, 37: 17-23.

## Examples

``` r
A <- matrix(c(
  0, 0, 1, 1,
  0, 0, 1, 0,
  1, 0, 0, 0,
  1, 0, 1, 0
), byrow = TRUE, ncol = 4)
V <- c(2, 2, 1, 1)

spatial_cor(A, V, measures = c("moran"))
#> [1] -0.6666667
```
