# Unipartite projections

Two-mode networks can be represented (or 'projected') as one-mode
networks.

## Usage

``` r
matrix_projection(A, B = NULL, digraph = FALSE)
```

## Arguments

- A:

  A first matrix object

- B:

  A second matrix object

- digraph:

  Whether the matrix is directed or not

## Value

This function return a list of matrices of the two projections of the
original matrix.

## References

Davis, Allison; Gardner, Burleigh B. and Mary. R. Gardner (1941). Deep
South: A Social Anthropological Study of Caste and Class. The University
of Chicago Press, Chicago.

Breiger, Ronald L. (1976). The Duality of Persons and Groups, 53(2),
181-190 [doi:10.2307/2576011](https://doi.org/10.2307/2576011)

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  2, 0, 2,
  1, 1, 0,
  0, 3, 3,
  0, 2, 2,
  0, 0, 1
), byrow = TRUE, ncol = 3)
matrix_projection(A)
#> $matrix1
#>      [,1] [,2] [,3]
#> [1,]    5    1    4
#> [2,]    1   14   13
#> [3,]    4   13   18
#> 
#> $matrix2
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    8    2    6    4    2
#> [2,]    2    2    3    2    0
#> [3,]    6    3   18   12    3
#> [4,]    4    2   12    8    2
#> [5,]    2    0    3    2    1
#> 

A <- matrix(c(
  0, 0, 0, 0, 1,
  1, 0, 0, 0, 0,
  1, 1, 0, 0, 0,
  0, 1, 1, 1, 1,
  0, 0, 1, 0, 0,
  0, 0, 1, 1, 0
), byrow = TRUE, ncol = 5)

B <- matrix(c(
  0, 0, 0, 0, 1,
  1, 0, 0, 0, 0,
  1, 0, 0, 0, 0,
  0, 1, 0, 0, 0,
  0, 0, 1, 0, 0,
  0, 0, 1, 0, 0
), byrow = TRUE, ncol = 5)
matrix_projection(A, B, digraph = TRUE)
#> $matrix1
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    2    1    0    0    0
#> [2,]    0    1    1    1    1
#> [3,]    0    0    2    1    0
#> [4,]    0    0    0    0    0
#> [5,]    0    0    0    0    1
#> 
#> $matrix2
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    1    0    0    1    0    0
#> [2,]    0    1    1    0    0    0
#> [3,]    0    1    1    0    0    0
#> [4,]    0    0    1    1    0    0
#> [5,]    0    0    0    1    1    1
#> [6,]    0    0    0    1    1    1
#> 
```
