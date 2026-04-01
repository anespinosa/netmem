# Z test of the number of arcs

Z test of the number of arcs

## Usage

``` r
z_arctest(G, p = 0.5, interval = FALSE)
```

## Arguments

- G:

  A symmetric matrix object.

- p:

  Constant probability p.

- interval:

  Return a 95 percent confidence interval.

## Value

This function gives a Z test and p-value for the number of lines or arcs
present in a directed graph

## References

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
data(krackhardt_friends)
z_arctest(krackhardt_friends)
#>      z      p 
#> -10.54   0.00 
```
