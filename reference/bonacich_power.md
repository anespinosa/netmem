# Bonacich power centrality

Power centrality of Bonacich (1987), where being connected to
well-connected others can increase or decrease the centrality of a node.

## Usage

``` r
bonacich_power(
  A,
  beta = 0,
  digraph = TRUE,
  weighted = FALSE,
  scale = c("none", "ssq")
)
```

## Arguments

- A:

  A square matrix

- beta:

  The weight given to the centrality of the neighbours. It should be
  smaller than the inverse of the leading eigenvalue

- digraph:

  Whether the matrix is directed or undirected

- weighted:

  Whether the matrix is weighted

- scale:

  Whether the scores are returned as they are (`none`, default) or
  scaled so that the sum of their squares is the number of nodes
  (`ssq`), as in other packages

## Value

This function returns the power centrality of the nodes.

## Details

The centrality is \\x = (I - \beta A)^{-1} A 1\\. When `beta` is
positive, a node is central when it is connected to central nodes, as in
the eigenvector centrality. When `beta` is negative, being connected to
well-connected others reduces the centrality of a node, which describes
bargaining situations. With `beta = 0` the measure is the degree.

## References

Bonacich, P. (1987). Power and centrality: A family of measures.
American Journal of Sociology, 92(5), 1170–1182.
[doi:10.1086/228631](https://doi.org/10.1086/228631)

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

bonacich_power(A, beta = 0.1, digraph = FALSE)
#>        a        b        c        d        e 
#> 3.518017 1.351802 2.476567 1.351802 1.247657 
bonacich_power(A, beta = -0.1, digraph = FALSE)
#>         a         b         c         d         e 
#> 2.6890231 0.7310977 1.6475734 0.7310977 0.8352427 
```
