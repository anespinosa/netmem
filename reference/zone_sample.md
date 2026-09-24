# Zone-2 sampling from second-mode

Second-zone multilevel sampling considering a second-mode focal actor

## Usage

``` r
zone_sample(A, X, ego = TRUE, core = FALSE)
```

## Arguments

- A:

  A symmetric matrix object.

- X:

  X an incidence matrix object.

- ego:

  Whether to add or not ego into the subgraph.

- core:

  Whether to add actors at distance one from ego

## Value

This function return a list of second-zone subgraphs using as a focal
actor the second-mode of the multilevel network. Each subgraph is the
binary adjacency matrix of the meta-matrix of `A` and `X` restricted to
the nodes of the zone, without loops. When `core = TRUE`, each matrix
has an attribute `core`, a named vector that is one for the actors at
distance one from the focal node and zero otherwise.

## References

Espinosa-Rada, A. (2021). A Network Approach for the Sociological Study
of Science: Modelling Dynamic Multilevel Networks.
\[PhD\](https://research.manchester.ac.uk/en/studentTheses/a-network-approach-for-the-sociological-study-of-science-and-know).
The University of Manchester.

## Author

Alejandro Espinosa-Rada

## Examples

``` r

A <- matrix(c(
  0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0,
  0, 1, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0
), byrow = TRUE, ncol = 8)
colnames(A) <- c("1", "2", "3", "4", "5", "6", "7", "8")
rownames(A) <- c("1", "2", "3", "4", "5", "6", "7", "8")

X <- matrix(c(
  1, 0, 0, 0,
  1, 0, 0, 0,
  1, 0, 1, 0,
  0, 1, 1, 0,
  0, 1, 1, 1,
  0, 1, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 1
), byrow = TRUE, ncol = 4)
colnames(X) <- c("a", "b", "c", "d")
rownames(X) <- c("1", "2", "3", "4", "5", "6", "7", "8")

set.seed(18051889)
zone_sample(A, X, core = TRUE)
#> $a
#>   1 2 3 4 a b c
#> 1 0 1 0 0 1 0 0
#> 2 0 0 1 0 1 0 0
#> 3 0 1 0 1 1 0 1
#> 4 0 0 0 0 0 1 1
#> a 1 1 1 0 0 0 0
#> b 0 0 0 1 0 0 0
#> c 0 0 1 1 0 0 0
#> attr(,"core")
#> 1 2 3 4 a b c 
#> 1 1 1 0 0 0 0 
#> 
#> $b
#>   3 4 5 6 a b c d
#> 3 0 1 0 0 1 0 1 0
#> 4 0 0 0 0 0 1 1 0
#> 5 0 0 0 0 0 1 1 1
#> 6 0 1 0 0 0 1 0 0
#> a 1 0 0 0 0 0 0 0
#> b 0 1 1 1 0 0 0 0
#> c 1 1 1 0 0 0 0 0
#> d 0 0 1 0 0 0 0 0
#> attr(,"core")
#> 3 4 5 6 a b c d 
#> 0 1 1 1 0 0 0 0 
#> 
#> $c
#>   2 3 4 5 6 a b c d
#> 2 0 1 0 0 0 1 0 0 0
#> 3 1 0 1 0 0 1 0 1 0
#> 4 0 0 0 0 0 0 1 1 0
#> 5 0 0 0 0 0 0 1 1 1
#> 6 0 0 1 0 0 0 1 0 0
#> a 1 1 0 0 0 0 0 0 0
#> b 0 0 1 1 1 0 0 0 0
#> c 0 1 1 1 0 0 0 0 0
#> d 0 0 0 1 0 0 0 0 0
#> attr(,"core")
#> 2 3 4 5 6 a b c d 
#> 0 1 1 1 0 0 0 0 0 
#> 
#> $d
#>   5 8 b c d
#> 5 0 0 1 1 1
#> 8 0 0 0 0 1
#> b 1 0 0 0 0
#> c 1 0 0 0 0
#> d 1 1 0 0 0
#> attr(,"core")
#> 5 8 b c d 
#> 1 1 0 0 0 
#> 
```
