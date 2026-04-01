# Ego network

Submatrix of ego's neighbourhoods

## Usage

``` r
ego_net(
  A,
  ego = NULL,
  bipartite = FALSE,
  addEgo = FALSE,
  select = c("all", "in", "out")
)
```

## Arguments

- A:

  A symmetric matrix object

- ego:

  Name of ego in the matrix

- bipartite:

  Whether the matrix is a two-mode network

- addEgo:

  Whether to retain ego in the submatrix or not

- select:

  Whether to consider all sender and receiver ties of ego (`all`), only
  incoming ties (`in`), or outgoing ties (`out`). By default, `all`.

## Value

This function returns redundancy, effective size and efficiency measures
(Burt, 1992).

## References

Burt, R.S., 1992. Structural Holes: the Social Structure of Competition.
Harvard University Press, Cambridge.

Borgatti, S., 1997. Unpacking Burt's redundancy measure. Connections,
20(1): 35-38.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 0, 0, 1, 1, 1,
  1, 0, 0, 1, 0, 0, 1,
  0, 0, 0, 0, 0, 0, 1,
  0, 1, 0, 0, 0, 0, 1,
  1, 0, 0, 0, 0, 0, 1,
  1, 0, 0, 0, 0, 0, 1,
  1, 1, 1, 1, 1, 1, 0
), ncol = 7, byrow = TRUE)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]
ego_net(A, ego = "g")
#>   a b c d e f
#> a 0 1 0 0 1 1
#> b 1 0 0 1 0 0
#> c 0 0 0 0 0 0
#> d 0 1 0 0 0 0
#> e 1 0 0 0 0 0
#> f 1 0 0 0 0 0
```
