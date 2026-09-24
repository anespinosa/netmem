# Core-periphery structure

Core-periphery model of Borgatti and Everett (2000): a group of nodes
that are connected among themselves and with the rest, and a periphery
of nodes that are connected with the core but not with each other.

## Usage

``` r
core_periphery(
  A,
  method = c("discrete", "continuous"),
  digraph = FALSE,
  rep = 50
)
```

## Arguments

- A:

  A square matrix

- method:

  Whether to return a `discrete` partition (default) or `continuous`
  coreness scores

- digraph:

  Whether the matrix is directed or undirected

- rep:

  Number of random partitions used to start the search, besides the one
  given by the degree of the nodes

## Value

This function returns the fit of the model, and the partition or the
coreness scores.

## Details

The `discrete` model looks for the partition of the nodes into a core
and a periphery that maximises the correlation between the observed
matrix and the ideal pattern, where a tie is expected when at least one
of the two nodes belongs to the core. The search starts from the nodes
sorted by degree, and from `rep` random partitions, and then moves one
node at a time while the correlation improves. As the search can end in
a local optimum, the result of the random starts depends on the seed.

The `continuous` model gives each node a coreness score instead of a
class. The scores maximise the correlation between the observed matrix
and the products of the scores of each pair, and are given by the
leading eigenvector of the matrix.

## References

Borgatti, S. P. and Everett, M. G. (2000). Models of core/periphery
structures. Social Networks, 21(4), 375–395.
[doi:10.1016/S0378-8733(99)00019-2](https://doi.org/10.1016/S0378-8733%2899%2900019-2)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 1, 1, 0,
  1, 0, 1, 1, 0, 1,
  1, 1, 0, 1, 0, 0,
  1, 1, 1, 0, 0, 0,
  1, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0
), byrow = TRUE, ncol = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

core_periphery(A)
#> $fit
#> [1] 0.6000992
#> 
#> $core
#> [1] "a" "b"
#> 
#> $periphery
#> [1] "c" "d" "e" "f"
#> 
#> $class
#> [1] "core"      "core"      "periphery" "periphery" "periphery" "periphery"
#> 
core_periphery(A, method = "continuous")
#> $fit
#> [1] 0.7804686
#> 
#> $coreness
#>         a         b         c         d         e         f 
#> 0.5058703 0.5058703 0.4674790 0.4674790 0.1598706 0.1598706 
#> 
```
