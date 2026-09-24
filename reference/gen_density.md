# Generalized density

Generalized density

## Usage

``` r
gen_density(
  A,
  directed = TRUE,
  bipartite = FALSE,
  loops = FALSE,
  weighted = FALSE,
  multilayer = FALSE
)
```

## Arguments

- A:

  A symmetric or incidence matrix object

- directed:

  Whether the matrix is directed

- bipartite:

  Whether the matrix is bipartite

- loops:

  Whether to consider the loops

- weighted:

  Whether the matrix is weighted

- multilayer:

  Whether the matrix is multilayer (i.e., multiplex and/or multilevel)

## Value

This function returns the density of the matrix(es)

## Details

The density is the number of ties divided by the number of possible
ties: \\n(n - 1)\\ for a directed network, \\n(n - 1)/2\\ for an
undirected one, and \\nm\\ for a two-mode network of \\n\\ and \\m\\
nodes. With `loops = TRUE` the diagonal is counted among the possible
ties of a one-mode network. With `directed = FALSE`, a tie in either
direction is an edge of the underlying graph.

In a list of matrices (`multilayer = TRUE`), the rectangular matrices
are taken as two-mode networks and the square ones as one-mode networks,
so a square incidence matrix should be given on its own with
`bipartite = TRUE`.

## References

Wasserman, S., and Faust, K. (1994). Social Network Analysis: Methods
and Applications. Cambridge: Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r

# A bipartite matrix
B <- matrix(c(
  1, 1, 0,
  0, 0, 1,
  0, 1, 1,
  0, 0, 1
), byrow = TRUE, ncol = 3)
gen_density(B, bipartite = TRUE)
#> [1] 0.5

# A multilevel network
A1 <- matrix(c(
  0, 1, 0, 0, 1,
  1, 0, 0, 1, 1,
  0, 0, 0, 1, 1,
  0, 1, 1, 0, 1,
  1, 1, 1, 1, 0
), byrow = TRUE, ncol = 5)

B1 <- matrix(c(
  1, 0, 0,
  1, 1, 0,
  0, 1, 0,
  0, 1, 0,
  0, 1, 1
), byrow = TRUE, ncol = 3)

A2 <- matrix(c(
  0, 1, 1,
  1, 0, 0,
  1, 0, 0
), byrow = TRUE, nrow = 3)

B2 <- matrix(c(
  1, 1, 0, 0,
  0, 0, 1, 0,
  0, 0, 1, 1
), byrow = TRUE, ncol = 4)

A3 <- matrix(c(
  0, 1, 3, 1,
  1, 0, 0, 0,
  3, 0, 0, 5,
  1, 0, 5, 0
), byrow = TRUE, ncol = 4)

matrices <- list(A1, B1, A2, B2, A3)
gen_density(matrices, multilayer = TRUE)
#> Warning: The matrix in [[5]] is valued
#> $`Density of matrix [[1]]`
#> [1] 0.7
#> 
#> $`Density of matrix [[2]]`
#> [1] 0.4666667
#> 
#> $`Density of matrix [[3]]`
#> [1] 0.6666667
#> 
#> $`Density of matrix [[4]]`
#> [1] 0.4166667
#> 
#> $`Density of matrix [[5]]`
#> [1] NA
#> 

# A multiplex network
A <- matrix(c(
  0, 1, 3, 6, 4,
  2, 0, 4, 5, 2,
  4, 1, 0, 6, 1,
  5, 6, 3, 0, 6,
  1, 1, 2, 3, 0
), byrow = TRUE, ncol = 5)
gen_density(A, multilayer = TRUE)
#> $`Density of matrix [[1]]`
#> [1] 0.25
#> 
#> $`Density of matrix [[2]]`
#> [1] 0.15
#> 
#> $`Density of matrix [[3]]`
#> [1] 0.15
#> 
#> $`Density of matrix [[4]]`
#> [1] 0.15
#> 
#> $`Density of matrix [[5]]`
#> [1] 0.1
#> 
#> $`Density of matrix [[6]]`
#> [1] 0.2
#> 
```
