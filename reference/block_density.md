# Block densities and image matrix

Densities of the blocks given by a partition of the nodes, and the image
matrix that summarises them (Lorrain and White, 1971; Wasserman and
Faust, 1994).

## Usage

``` r
block_density(A, partition, cutoff = "density", loops = FALSE)
```

## Arguments

- A:

  A square matrix

- partition:

  A vector with the position of each node

- cutoff:

  Density above which a block is a one in the image matrix: a number, or
  `density` (default) for the density of the whole network

- loops:

  Whether the loops are counted as possible ties in the diagonal blocks

## Value

This function returns the density of each block, the image matrix and
the density of the network.

## Details

A partition of the nodes divides the matrix into blocks. The density of
a block is the proportion of the possible ties that are present, and the
image matrix assigns a one to the blocks whose density is at least the
`cutoff`. The usual criterion is the density of the whole network
(`cutoff = "density"`), so a block is one when it is denser than the
network as a whole.

The diagonal blocks contain the ties within a position, where the
possible ties exclude the loops unless `loops = TRUE`.

## References

Lorrain, F. and White, H. C. (1971). Structural equivalence of
individuals in social networks. Journal of Mathematical Sociology, 1(1),
49–80.
[doi:10.1080/0022250X.1971.9989788](https://doi.org/10.1080/0022250X.1971.9989788)

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 0, 0, 0,
  1, 1, 0, 1, 1, 1,
  0, 0, 1, 0, 0, 0,
  0, 0, 1, 0, 0, 0,
  0, 0, 1, 0, 0, 0
), byrow = TRUE, ncol = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

block_density(A, partition = c(1, 1, 1, 2, 2, 2))
#> $densities
#>           1         2
#> 1 1.0000000 0.3333333
#> 2 0.3333333 0.0000000
#> 
#> $image
#>   1 2
#> 1 1 0
#> 2 0 0
#> 
#> $density
#> [1] 0.4
#> 
```
