# Matrix report

The primary matrix used in social network analysis are the adjacency
matrix or sociomatrix, and the incidence matrix.

## Usage

``` r
matrix_report(A)
```

## Arguments

- A:

  A matrix

## Value

This function return a report of some of the characteristics of the
matrix.

## References

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  1, 1, 0, 0, -1,
  1, 0, 0, 1, 1,
  0, 0, NA, 1, 1,
  0, 1, 1, 0, 1,
  1, 1, 1, 1, 0
), byrow = TRUE, ncol = 5)

B <- matrix(c(
  1, 0, 0,
  1, 1, 0,
  0, NA, 0,
  0, 1, 0,
  0, 1, 1
), byrow = TRUE, ncol = 3)
matrix_report(A)
#> The matrix A might have the following characteristics:
#> --> The vectors of the matrix are `numeric`
#> --> No names assigned to the rows of the matrix
#> --> No names assigned to the columns of the matrix
#> --> The matrix has negative elements (network is signed)
#> --> The matrix has NA elements
#> --> Matrix is asymmetric (network is directed)
#> --> The main diagonal is nonzero (the network has loops)
#> --> The matrix is square, 5 by 5 
#>      nodes arcs
#> [1,]     5   13
matrix_report(B)
#> The matrix A might have the following characteristics:
#> --> The vectors of the matrix are `numeric`
#> --> No names assigned to the rows of the matrix
#> --> No names assigned to the columns of the matrix
#> --> The matrix has NA elements
#> --> The matrix is rectangular, 3 by 5 
#>      nodes_rows nodes_columns incidence_lines
#> [1,]          3             5               6
```
