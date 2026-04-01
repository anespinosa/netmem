# Extract components

This function extract the matrix of different components

## Usage

``` r
extract_component(A, maximum = TRUE, position = NULL)
```

## Arguments

- A:

  A matrix

- maximum:

  Whether to extract the maximum component

- position:

  Whether to extract the component in the ith size position

## Value

A matrix or a list of matrices with the required components

## References

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- FIFAex$Matrix
rownames(A) <- FIFAex$label
colnames(A) <- rownames(A)
extract_component(A, maximum = TRUE)
#>                      IFAB FIFA British Associations CAS WADA Countries
#> IFAB                    0    1                    1   0    0         0
#> FIFA                    1    0                    1   1    1         1
#> British Associations    1    1                    0   0    0         0
#> CAS                     0    1                    0   0    1         0
#> WADA                    0    1                    0   1    0         0
#> Countries               0    1                    0   0    0         0
extract_component(A, maximum = FALSE, position = 2)
#>                Olympics Games
#> Olympics Games              0
```
