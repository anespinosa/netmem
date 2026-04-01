# Expand Matrix

Expand Matrix

## Usage

``` r
expand_matrix(A, label = NULL, loops = FALSE, normalize = FALSE)
```

## Arguments

- A:

  A square matrix

- label:

  Duplicated labels to expand the matrix

- loops:

  Whether the loops are retained or not

- normalize:

  Whether to normalize the matrix considering the fractional counting
  per group

## Value

Return an expanded matrix

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1,
  0, 0, 1,
  1, 0, 0
), byrow = TRUE, ncol = 3, nrow = 3)
rownames(A) <- letters[1:NROW(A)]
colnames(A) <- rownames(A)
label <- sort(rep(rownames(A), 2))
expand_matrix(A, label, loops = FALSE, normalize = TRUE)
#>   [,1] [,2] [,3] [,4] [,5] [,6]
#> a  0.0  0.0  0.5  0.5  0.5  0.5
#> a  0.0  0.0  0.5  0.5  0.5  0.5
#> b  0.0  0.0  0.0  0.0  0.5  0.5
#> b  0.0  0.0  0.0  0.0  0.5  0.5
#> c  0.5  0.5  0.0  0.0  0.0  0.0
#> c  0.5  0.5  0.0  0.0  0.0  0.0
```
