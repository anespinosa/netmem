# PageRank centrality

PageRank of Brin and Page (1998): the probability that a random walker,
who follows the ties and sometimes jumps to a random node, is found in
each node.

## Usage

``` r
page_rank_centrality(
  A,
  damping = 0.85,
  digraph = TRUE,
  weighted = FALSE,
  tol = 1e-10,
  max_iter = 1000
)
```

## Arguments

- A:

  A square matrix

- damping:

  Probability of following a tie instead of jumping to a random node

- digraph:

  Whether the matrix is directed or undirected

- weighted:

  Whether the matrix is weighted

- tol:

  Tolerance of the power iteration

- max_iter:

  Maximum number of iterations

## Value

This function returns the PageRank of the nodes, which adds up to one.

## Details

At each step, the walker follows one of the outgoing ties of the node
with probability `damping`, and jumps to a node chosen at random with
probability `1 - damping`. The nodes without outgoing ties are treated
as if they were connected to every node.

## References

Brin, S. and Page, L. (1998). The anatomy of a large-scale hypertextual
Web search engine. Computer Networks and ISDN Systems, 30(1-7), 107–117.
[doi:10.1016/S0169-7552(98)00110-X](https://doi.org/10.1016/S0169-7552%2898%2900110-X)

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

page_rank_centrality(A, digraph = FALSE)
#>         a         b         c         d         e 
#> 0.3575578 0.1313080 0.2454920 0.1313080 0.1343341 
```
