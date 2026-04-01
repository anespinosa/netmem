# Power matrix

Power of a matrix computed by successive matrix multiplication.

## Usage

``` r
power_function(A, n)
```

## Arguments

- A:

  A matrix

- n:

  Positive integer

## Value

This function return the power of a matrix by repeating matrix
multiplication.

## References

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  1, 0, 0, 0,
  1, 1, 0, 0,
  1, 0, 1, 0,
  0, 1, 1, 1
), byrow = TRUE, ncol = 4, nrow = 4)
power_function(A, 1000)
#>        [,1] [,2] [,3] [,4]
#> [1,]      1    0    0    0
#> [2,]   1000    1    0    0
#> [3,]   1000    0    1    0
#> [4,] 999000 1000 1000    1
```
