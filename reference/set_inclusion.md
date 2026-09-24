# Set inclusion of neighbourhoods

Inclusion of the neighbourhoods of the rows of an incidence matrix.

## Usage

``` r
set_inclusion(N, M = N, proper = FALSE)
```

## Arguments

- N:

  An incidence matrix of the neighbourhoods to be included

- M:

  An incidence matrix, with the same dimensions as `N`, of the including
  neighbourhoods

- proper:

  Whether the inclusion should be proper

## Value

This function returns a binary matrix `P` where `P[u, v] = 1` if `N(u)`
is included in `M(v)`.

## Details

The neighbourhoods of the rows can be defined on any set of columns
(e.g. papers of authors, or events of actors). `N` contains the
neighbourhood that should be included, and `M` the neighbourhood in
which it should be included. `M` is the same as `N` for open
neighbourhoods, or can add other elements, such as the node itself, for
closed neighbourhoods.

With `proper = TRUE` the inclusion should be proper, \\N(u) \subsetneq
M(v)\\, i.e. \\M(v)\\ has at least one element that is not in \\N(u)\\.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
X <- matrix(c(
  1, 1, 1, 0,
  1, 1, 0, 0,
  1, 0, 0, 0,
  0, 1, 0, 0,
  0, 0, 1, 1
), byrow = TRUE, ncol = 4)
rownames(X) <- c("a1", "a2", "a3", "a4", "a5")
colnames(X) <- c("w1", "w2", "w3", "w4")

set_inclusion(X)
#>    a1 a2 a3 a4 a5
#> a1  0  0  0  0  0
#> a2  1  0  0  0  0
#> a3  1  1  0  0  0
#> a4  1  1  0  0  0
#> a5  0  0  0  0  0
```
