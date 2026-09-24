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

  The position of the size of the component, from the largest (1). Used
  when `maximum = FALSE`

## Value

The matrix of the component, or a list with the matrices of the
components when several have the same size

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
