# Transform a matrix to an adjacency list

Transform a matrix to an adjacency list

## Usage

``` r
matrix_adjlist(A)
```

## Arguments

- A:

  A matrix

## Value

This function transform a matrix to an adjacency list

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0, 0, 1, 0,
  1, 0, 1, 0, 0, 0, 0, 0, 0,
  1, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 1, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 1, 1, 0,
  0, 0, 0, 0, 0, 1, 0, 1, 0,
  1, 0, 0, 0, 0, 1, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0
), byrow = TRUE, ncol = 9)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)
matrix_adjlist(A)
#> $a
#> [1] "b" "c" "h"
#> 
#> $b
#> [1] "a" "c"
#> 
#> $c
#> [1] "a" "b"
#> 
#> $d
#> [1] "e" "f"
#> 
#> $e
#> [1] "d"
#> 
#> $f
#> [1] "d" "g" "h"
#> 
#> $g
#> [1] "f" "h"
#> 
#> $h
#> [1] "a" "f" "g"
#> 
#> $i
#> character(0)
#> 
```
