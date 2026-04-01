# Components

This function assigns an id to the components that each of the nodes of
the matrix belongs

## Usage

``` r
components_id(A)
```

## Arguments

- A:

  A matrix

## Value

A vector assigning an id the components that each of the nodes of the
matrix belongs

## References

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 0, 0,
  1, 1, 0, 0, 0,
  0, 0, 0, 0, 1,
  0, 0, 0, 1, 0
), byrow = TRUE, ncol = 5)
rownames(A) <- letters[1:ncol(A)]
colnames(A) <- rownames(A)
components_id(A)
#> $components
#> [1] 1 1 1 2 2
#> 
#> $size
#> components
#> 1 2 
#> 3 2 
#> 
```
