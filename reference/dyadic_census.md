# Dyad census

Dyad census

## Usage

``` r
dyadic_census(G, directed = TRUE, loops = FALSE)
```

## Arguments

- G:

  A symmetric matrix object.

- directed:

  Whether the matrix is directed or not

- loops:

  Whether to expect nonzero elements in the diagonal of the matrix

## Value

This function return the counts of the dyad census.

## References

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
data(krackhardt_friends)
dyadic_census(krackhardt_friends)
#>      Mutual Asymmetrics       Nulls 
#>          23          56         131 

data(FIFAin)
dyadic_census(FIFAin[[1]], directed = FALSE)
#> Mutual  Nulls 
#>    111    709 
```
