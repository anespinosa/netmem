# Eigenvector centrality

Eigenvector centrality of Bonacich (1972), the leading eigenvector of
the matrix.

## Usage

``` r
eigenvector_centrality(
  A,
  digraph = TRUE,
  type = c("in", "out"),
  weighted = FALSE,
  scale = c("max", "unit"),
  signed = FALSE
)
```

## Arguments

- A:

  A square matrix

- digraph:

  Whether the matrix is directed or undirected

- type:

  Whether to use the `in` (default) or `out` ties for directed networks

- weighted:

  Whether the matrix is weighted

- scale:

  Whether the vector is scaled with a maximum of one (`max`, default) or
  has unit length (`unit`)

- signed:

  Whether the matrix has negative ties (Bonacich and Lloyd, 2004). The
  scores can then be negative, and the eigenvector is the one of the
  eigenvalue with the largest absolute value

## Value

This function returns the eigenvector centrality of the nodes and the
leading eigenvalue.

## Details

A node is central when it is connected to other central nodes. For
directed networks, the `in` option gives centrality to the nodes that
receive ties from central nodes, and `out` to the nodes that send ties
to central nodes.

## References

Bonacich, P. (1972). Factoring and weighting approaches to status scores
and clique identification. Journal of Mathematical Sociology, 2(1),
113–120.
[doi:10.1080/0022250X.1972.9989806](https://doi.org/10.1080/0022250X.1972.9989806)

Bonacich, P. (1987). Power and centrality: A family of measures.
American Journal of Sociology, 92(5), 1170–1182.
[doi:10.1086/228631](https://doi.org/10.1086/228631)

Bonacich, P. and Lloyd, P. (2004). Calculating status with negative
relations. Social Networks, 26(4), 331–338.
[doi:10.1016/j.socnet.2004.08.007](https://doi.org/10.1016/j.socnet.2004.08.007)

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

eigenvector_centrality(A, digraph = FALSE)
#> $vector
#>         a         b         c         d         e 
#> 1.0000000 0.5411961 0.7653669 0.5411961 0.4142136 
#> 
#> $value
#> [1] 1.847759
#> 
eigenvector_centrality(A, digraph = FALSE, scale = "unit")
#> $vector
#>         a         b         c         d         e 
#> 0.6532815 0.3535534 0.5000000 0.3535534 0.2705981 
#> 
#> $value
#> [1] 1.847759
#> 

# With negative ties the status of a node can be negative
S <- matrix(c(
  0, 1, 1, -1,
  1, 0, 1, -1,
  1, 1, 0, -1,
  -1, -1, -1, 0
), byrow = TRUE, ncol = 4)
rownames(S) <- letters[1:nrow(S)]
colnames(S) <- rownames(S)

eigenvector_centrality(S, digraph = FALSE, signed = TRUE)
#> $vector
#>  a  b  c  d 
#>  1  1  1 -1 
#> 
#> $value
#> [1] 3
#> 
```
