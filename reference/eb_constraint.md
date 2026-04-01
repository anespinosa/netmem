# Constraint

Everett and Borgatti specification of the constraint measure for binary
matrices

## Usage

``` r
eb_constraint(A, ego = NULL, digraph = FALSE, weighted = FALSE)
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

This function returns term 1, 2 and 3, the normalization and the maximum
value of the specification of Everett and Borgatti (2020), and the
constraint of Burt (1992).

## References

Burt, R.S., 1992. Structural Holes: the Social Structure of Competition.
Harvard University Press, Cambridge.

Everett, M.G. and Borgatti, S., 2020. Unpacking Burt's constraint
measure. Social Networks 62, pp. 50-57.
[doi:10.1016/j.socnet.2020.02.001](https://doi.org/10.1016/j.socnet.2020.02.001)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 1,
  1, 0, 1, 0, 0, 1,
  1, 1, 0, 0, 0, 1,
  0, 0, 0, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  1, 1, 1, 1, 1, 0
), ncol = 6, byrow = TRUE)

rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]
eb_constraint(A, ego = "f")
#> $results
#>   term1 term2 term3 constraint normalization
#> f   0.2  0.24 0.073      0.513         0.699
#> 
#> $maximum
#>     f 
#> 0.648 
#> 
```
