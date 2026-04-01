# Clique percolation

Clique Percolation Method (CPM) is an algorithm for finding overlapping
communities within networks, introduced by Palla et al. (2005). This
function firstly identify cliques of size k, then creates a incidence
matrix as an affiliation network.

## Usage

``` r
percolation_clique(A)
```

## Arguments

- A:

  A matrix

## Value

A matrix that assign each node to a clique

## References

Palla, G., Derényi, I., Farkas, I., & Vicsek, T. (2005). Uncovering the
overlapping community structure of complex networks in nature and
society. Nature, 435(7043), 814-818.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(
  c(
    0, 1, 1, 1, 0, 0, 0, 0, 0,
    1, 0, 1, 0, 0, 0, 0, 0, 0,
    1, 1, 0, 1, 0, 0, 0, 0, 0,
    1, 0, 1, 0, 1, 1, 0, 0, 0,
    0, 0, 0, 1, 0, 1, 1, 1, 0,
    0, 0, 0, 1, 1, 0, 1, 1, 0,
    0, 0, 0, 0, 1, 1, 0, 1, 1,
    0, 0, 0, 0, 1, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 1, 0, 0
  ),
  byrow = TRUE, ncol = 9
)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]
percolation_clique(A)
#>   1 1 2 2 2 2 2
#> a 1 1 0 0 0 0 0
#> b 1 0 0 0 0 0 0
#> c 1 1 0 0 0 0 0
#> d 0 1 1 0 0 0 0
#> e 0 0 1 1 1 1 0
#> f 0 0 1 1 1 0 1
#> g 0 0 0 1 0 1 1
#> h 0 0 0 0 1 1 1
#> i 0 0 0 0 0 0 0
```
