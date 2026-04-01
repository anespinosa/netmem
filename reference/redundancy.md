# Redundancy measures

Redundancy measures of the structural holes theory for binary matrixes

## Usage

``` r
redundancy(A, ego = NULL, digraph = FALSE, weighted = FALSE)
```

## Arguments

- A:

  A symmetric matrix object

- ego:

  Name of ego in the matrix

- digraph:

  Whether the matrix is directed or undirected

- weighted:

  Whether the matrix is weighted or not

## Value

This function returns redundancy, effective size and efficiency measures
(Burt, 1992).

## References

Burt, R.S., 1992. Structural Holes: the Social Structure of Competition.
Harvard University Press, Cambridge.

Borgatti, S., 1997. Unpacking Burt's redundancy measure. Connections,
20(1): 35-38.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 0, 0, 1, 1, 1,
  1, 0, 0, 1, 0, 0, 1,
  0, 0, 0, 0, 0, 0, 1,
  0, 1, 0, 0, 0, 0, 1,
  1, 0, 0, 0, 0, 0, 1,
  1, 0, 0, 0, 0, 0, 1,
  1, 1, 1, 1, 1, 1, 0
), ncol = 7, byrow = TRUE)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]
redundancy(A, ego = "g")
#> $redundancy
#> [1] 1.333333
#> 
#> $effective_size
#> [1] 4.666667
#> 
#> $efficiency
#> [1] 0.7777778
#> 
```
