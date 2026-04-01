# Blau's and IQV index

This index was used by Blau (1977) to distinguish between the relative
prevalence of between and within-group ties. This measure can be
interpreted as heterogeneity at the network level.

## Usage

``` r
heterogeneity(att, normalized = FALSE)
```

## Arguments

- att:

  Categorical attribute of the nodes

- normalized:

  Whether to return IQV index

## Value

Numerical value of the Blau index.

If `normalized = TRUE`, then the function also return IQV index.

## References

Agresti, A. and Agresti, B. (1978). Statistical Analysis of Qualitative
Variation. Sociological Methodology, 9, 204-237.
[doi:10.2307/270810](https://doi.org/10.2307/270810)

Blau, P. M. (1977). Inequality and heterogeneity. New York: Free Press.

## Examples

``` r
a <- rep(1:10, 10)
heterogeneity(a, normalized = TRUE)
#> $blau
#>   1 
#> 0.9 
#> 
#> $iqv
#> 1 
#> 1 
#> 

a <- rep(1:2, 10)
heterogeneity(a, normalized = TRUE)
#> $blau
#>   1 
#> 0.5 
#> 
#> $iqv
#> 1 
#> 1 
#> 
```
