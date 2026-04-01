# Geographical distances

This function calculate some geographical distances considering a list
of places specifying their latitud and longitud. The function currently
works for degree decimal or radians formats.

## Usage

``` r
dist_geographic(
  latitude,
  longitud,
  method = c("spherical", "harvesine", "manhattan", "minkowski"),
  places = NULL,
  dd_to_radians = FALSE,
  p = NULL
)
```

## Source

Adapted from Mario Pineda-Krch (Great-circle distance calculations in R)

## Arguments

- latitude:

  A vector with latitude

- longitud:

  A vector with longitud

- method:

  Whether to use the Spherical Law of Cosines `spherical` (default),
  Haversine formula `harvesine`, Manhattan Distance `manhattan` or
  Minkowski distance `minkowski`

- places:

  A vector with the names of the places

- dd_to_radians:

  Whether to transform degree decimal format to radians

- p:

  Parameter p for the estimation of Minkowski distance (default = 2,
  which is equivalent to an Euclidian Distance)

## Value

This function return a distance matrix.

## Examples

``` r
set.seed(1234)
x <- cbind(latitud = rnorm(5, -90), longitud = rnorm(5, 45))
dist_geographic(x[, 1], x[, 2], method = "harvesine")
#>           [,1]       [,2]      [,3]      [,4]       [,5]
#> [1,]     0.000 10082.8221 12409.829  8394.701 10610.5618
#> [2,] 10082.822     0.0000  5141.096 16711.983   980.0773
#> [3,] 12409.829  5141.0964     0.000 18176.087  4162.4257
#> [4,]  8394.701 16711.9829 18176.087     0.000 17689.1877
#> [5,] 10610.562   980.0773  4162.426 17689.188     0.0000
```
