# Katz centrality

Katz centrality (1953), which counts the paths that arrive to a node,
discounting the longer ones.

## Usage

``` r
katz_centrality(
  A,
  alpha = 0.1,
  beta = 1,
  type = c("in", "out"),
  digraph = TRUE,
  weighted = FALSE
)
```

## Arguments

- A:

  A square matrix

- alpha:

  The attenuation of the longer paths

- beta:

  The status that every node has independently of the network

- type:

  Whether the paths considered are the ones that arrive (`in`, default)
  or leave (`out`) the node

- digraph:

  Whether the matrix is directed or undirected

- weighted:

  Whether the matrix is weighted

## Value

This function returns the Katz centrality of the nodes.

## Details

The centrality is \\x = \beta (I - \alpha A^T)^{-1} 1\\, so a path of
length \\k\\ contributes \\\alpha^k\\. The attenuation `alpha` should be
smaller than the inverse of the leading eigenvalue of the matrix,
otherwise the sum does not converge.

## References

Katz, L. (1953). A new status index derived from sociometric analysis.
Psychometrika, 18(1), 39–43.
[doi:10.1007/BF02289026](https://doi.org/10.1007/BF02289026)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 1, 0,
  1, 0, 0, 0, 0,
  1, 0, 0, 0, 1,
  1, 0, 0, 0, 0,
  0, 0, 1, 0, 0
), byrow = TRUE, ncol = 5)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

katz_centrality(A, alpha = 0.1, digraph = FALSE)
#>        a        b        c        d        e 
#> 1.351802 1.135180 1.247657 1.135180 1.124766 
```
