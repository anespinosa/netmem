# Transform a square matrix to an edge-list

Transform a square matrix to an edge-list

## Usage

``` r
matrix_to_edgelist(A, digraph = FALSE, valued = FALSE, loops = FALSE)
```

## Arguments

- A:

  A square matrix

- digraph:

  Whether the matrix is directed or not

- valued:

  Add a third columns with the valued of the relationship

- loops:

  Whether the loops are retained or not

## Value

This function transform the matrix into an edgelist

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 2, 1,
  1, 0, 0,
  1, 0, 1
), byrow = TRUE, ncol = 3)
matrix_to_edgelist(A, digraph = TRUE, valued = TRUE, loops = TRUE)
#>      [,1] [,2] [,3]
#> [1,] "2"  "1"  "1" 
#> [2,] "3"  "1"  "1" 
#> [3,] "1"  "2"  "2" 
#> [4,] "1"  "3"  "1" 
#> [5,] "3"  "3"  "1" 
```
