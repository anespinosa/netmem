# Transform an adjacency list into a matrix

Transform an adjacency list into a matrix

## Usage

``` r
adj_to_matrix(A, type = c("adjacency", "incidence", "weighted"), loops = FALSE)
```

## Arguments

- A:

  An adjacent list

- type:

  Transform the adjacent list into an `adjacency` matrix, an `incidence`
  matrix or a `weighted` matrix

- loops:

  Whether to include loops into the matrix

## Value

This function transforms an adjacency list into a matrix

## Author

Alejandro Espinosa-Rada

## Examples

``` r
adj_groups <- rbind(
  c("a", "b", "c"), c("a", "c", NA),
  c("b", "c", NA), c("c", NA, NA),
  c("c", "a", NA)
)
M <- adj_to_matrix(adj_groups, type = "adjacency", loops = TRUE)
M
#>   a a b c c
#> a 1 1 1 1 1
#> a 1 1 0 1 1
#> b 0 0 1 1 1
#> c 0 0 0 1 1
#> c 1 1 0 1 1
```
