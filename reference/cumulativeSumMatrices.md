# Cumulative sum of matrices

Cumulative sum of matrices

## Usage

``` r
cumulativeSumMatrices(matrixList)
```

## Arguments

- matrixList:

  A list of matrices

## Value

This function returns the cumulative sum of matrices

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1,
  0, 0, 0,
  0, 1, 0
), byrow = TRUE, ncol = 3)
B <- matrix(c(
  0, 0, 1,
  0, 0, 0,
  0, 0, 0
), byrow = TRUE, ncol = 3)
C <- matrix(c(
  0, 0, 0,
  1, 0, 0,
  0, 0, 0
), byrow = TRUE, ncol = 3)
matrixList <- list(A, B, C)
cumulativeSumMatrices(matrixList)
#> [[1]]
#>      [,1] [,2] [,3]
#> [1,]    0    1    1
#> [2,]    0    0    0
#> [3,]    0    1    0
#> 
#> [[2]]
#>      [,1] [,2] [,3]
#> [1,]    0    1    2
#> [2,]    0    0    0
#> [3,]    0    1    0
#> 
#> [[3]]
#>      [,1] [,2] [,3]
#> [1,]    0    1    2
#> [2,]    1    0    0
#> [3,]    0    1    0
#> 
```
