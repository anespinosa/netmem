# Shared partners

Shared partners

## Usage

``` r
shared_partners(
  A,
  loops = FALSE,
  directed = TRUE,
  type = c("dsp", "esp", "nsp")
)
```

## Arguments

- A:

  A binary matrix

- loops:

  Whether to consider the loops

- directed:

  Whether the matrix is directed

- type:

  Whether to return the `dyad-wise (dsp)` (default), `edge-wise (esp)`
  or `non-edgewise (nsp)` shared partners (Hunter and Handcock, 2006)

## Value

This function return the distribution of shared partners.

## References

Hunter, D. R. and M. S. Handcock (2006), Inference in curved exponential
family models for networks, Journal of Computational and Graphical
Statistics, 15: 565– 583.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 0, 0, 0, 0,
  1, 0, 1, 1, 0, 1,
  0, 1, 0, 1, 0, 0,
  0, 1, 1, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  0, 1, 0, 1, 1, 0
), byrow = TRUE, ncol = 6)
shared_partners(A, type = "dsp")
#> dsp
#>  0  1  2 
#>  4 18  8 
shared_partners(A, type = "esp")
#> 
#>  0  1  2 
#>  2 10  4 
shared_partners(A, type = "nsp")
#> nsp
#> 0 1 2 
#> 2 8 4 
```
