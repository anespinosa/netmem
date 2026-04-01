# Minimum/maximum overlap

Two-mode networks can be represented (or 'projected') as one-mode
networks.

## Usage

``` r
minmax_overlap(A, row = TRUE, min = TRUE)
```

## Arguments

- A:

  A matrix object

- row:

  Whether to consider the actors in the rows of the matrix (default) or
  the column.

- min:

  Whether to extract the minimum (default) or the maximum overlap.

## Value

This function return the overlap between the modes (a.k.a. actors,
nodes, vertices).

## References

Morris, S.A. (2005). Unified Mathematical Treatment of Complex Cascaded
Bipartite Networks: The Case of Collections of Journal Papers.
Unpublished PhD Thesis, Oklahoma State University.

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
minmax_overlap(A)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    4    1    2    2    1
#> [2,]    1    2    1    1    0
#> [3,]    2    1    6    4    1
#> [4,]    2    1    4    4    1
#> [5,]    1    0    1    1    1
```
