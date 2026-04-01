# Independent random matrix

The function creates random matrices following a uniform probability
over the space of networks having exactly m fixed number of edges
(Moreno and Jennings, 1938; Rapoport, 1948; Solomonoff and Rapoport,
1951; Erdos and Renyi, 1959) or following a probability of the formation
of the ties (Gilbert, 1959) assuming ties independency.

## Usage

``` r
ind_rand_matrix(
  n,
  m = NULL,
  type = c("edges", "probability"),
  digraph = TRUE,
  loops = FALSE,
  l = NULL,
  p = NULL,
  trials = 1,
  multilevel = FALSE
)
```

## Arguments

- n:

  The number of nodes of the first set

- m:

  The number of nodes of a second set

- type:

  The model assumes a fixed number of `edges` model (a.k.a. G(n,m))
  (default) or a `probability` model (a.k.a. G(n,p))

- digraph:

  Whether the matrix is symmetric or not

- loops:

  Whether to expect nonzero elements in the diagonal of the matrix

- l:

  The number of ties expected for the `edges` (a.k.a. G(n,m)) model

- p:

  The probability of the ties expected for the `probability` (a.k.a.
  G(n,p)) model. If no parameter \`p\` is specified, a uniform
  distribution is considered (p=0.5).

- trials:

  Whether to add counting numbers to the `probability` (a.k.a. G(n,p))
  model

- multilevel:

  Whether to return a meta-matrix to represent a multilevel network

## Value

This function return the counts of the dyad census.

## Details

The fixed model is often called the G(n,m) graph with 'n' nodes and 'm'
edges, and the 'm' edges are chosen uniformly randomly from the set of
all possible ties.

The probability model is known as the G(n,p) graph, in which the matrix
has 'n' nodes, and for each tie, the probability that it is present in
the matrix is 'p'.

These are the simplest models that follow a conditional uniform
distribution that place nonnull probability on a subset of networks with
distinctive characteristics corresponding to the observed networks - for
example, simulating a matrix based on the number of ties observed in the
network.

## References

Erdos, P. and Renyi, A. (1959). On random graphs. Publicationes
Mathematicae 6, 290–297.

Gilbert, N. (1959). Random Graphs. The Annals of Mathematical
Statistics, 30(4): 1141-1144.

Moreno, J. and Jennings, H. (1938). Statistics of social configurations.
Sociometry, 1(3/4):342–374.

Rapoport, A. (1948). Cycle distributions in random nets. Bulletin of
Mathematical Biology, 10(3):145–157.

Solomonoff, R. and Rapoport, A. (1951). Connectivity of random nets.
Bulletin of Mathematical Biology, 13:107–117.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
set.seed(18051889)
ind_rand_matrix(5, type = "edges", l = 3, digraph = TRUE, loops = TRUE)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0    0    0    0    0
#> [2,]    0    1    0    0    0
#> [3,]    0    1    0    0    0
#> [4,]    0    0    0    0    1
#> [5,]    0    0    0    0    0
ind_rand_matrix(5, type = "probability")
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0    1    0    1    0
#> [2,]    1    0    1    0    1
#> [3,]    0    1    0    1    1
#> [4,]    0    0    0    0    0
#> [5,]    0    1    1    1    0
ind_rand_matrix(n = 5, m = 2, p = 0.20, type = "probability", multilevel = TRUE)
#>    n1 n2 n3 n4 n5 m1 m2
#> n1  0  1  0  0  0  0  1
#> n2  0  0  1  1  0  0  0
#> n3  0  0  0  0  0  0  0
#> n4  0  0  0  0  0  0  0
#> n5  0  1  1  0  0  0  0
#> m1  0  0  0  0  0  0  0
#> m2  1  0  0  0  0  0  0
```
