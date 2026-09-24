# Simplicial complexes

Incidence matrix of the nodes of a network and the simplices of its
clique complex or of its neighbourhood complex.

## Usage

``` r
simplicial_complexes(
  A,
  zero_simplex = TRUE,
  projection = FALSE,
  complex = c("clique", "neighbourhood"),
  closed = FALSE,
  valued = FALSE
)
```

## Arguments

- A:

  A square matrix of a network, with names.

- zero_simplex:

  Whether to include the isolated nodes as simplices of dimension 0, for
  `complex = "clique"`.

- projection:

  Whether to return the links between the simplices through their shared
  nodes, and between the nodes through their shared simplices.

- complex:

  The complex: the maximal cliques (`clique`, default) or the
  neighbourhoods (`neighbourhood`).

- closed:

  Whether the neighbourhoods include the node itself, for
  `complex = "neighbourhood"`.

- valued:

  Whether the projections count the shared nodes or simplices instead of
  indicating whether there are any.

## Value

This function returns the incidence matrix of the nodes (rows) and the
simplices (columns). With `projection = TRUE`, a list with the incidence
matrix (`simplex`), the projection of the simplices (`projection1`) and
the projection of the nodes (`projection2`).

## Details

A simplex is a set of nodes, and a simplicial complex a collection of
simplices that contains every face of its simplices (Atkin, 1974). The
complex is represented by its maximal simplices, as the faces are
implied by the simplices that contain them.

With `complex = "clique"` (default), the simplices are the maximal
cliques of the underlying undirected network, so that a clique of four
nodes is a single simplex of dimension 3. The isolated nodes are cliques
of a single node, and they are included as simplices of dimension 0 when
`zero_simplex = TRUE`.

With `complex = "neighbourhood"`, each node is the simplex of its
neighbours, the rows of `A` (the out-neighbours of a directed network),
and with `closed = TRUE` the node is also a vertex of its own simplex
(Raj et al., 2024). The nodes without neighbours do not form a simplex
unless the neighbourhoods are closed.

The rows of the result are the nodes and the columns the simplices,
named after their nodes (`a-b-c`) for the clique complex, and after the
node whose neighbourhood they are (`N(a)` or `N[a]`) for the
neighbourhood complex. `q_analysis(t(S), simplicial_complex = TRUE)` is
the Q-analysis of the simplices, and
`q_analysis(S, simplicial_complex = TRUE)` that of the conjugate
complex, in which the nodes are connected through the simplices they
share.

## References

Atkin, R. H. (1974). Mathematical structure in human affairs. New York:
Crane, Rusak.

Freeman, L. C. (1980). Q-analysis and the structure of friendship
networks. International Journal of Man-Machine Studies, 12(4), 367–378.
[doi:10.1016/S0020-7373(80)80021-6](https://doi.org/10.1016/S0020-7373%2880%2980021-6)

Raj, U., Banerjee, A., Ray, S. and Bhattacharya, S. (2024). Structure of
higher-order interactions in social-ecological networks through
Q-analysis of their neighbourhood and clique complex. PLOS ONE, 19(8),
e0306409.
[doi:10.1371/journal.pone.0306409](https://doi.org/10.1371/journal.pone.0306409)

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0, 0, 1, 0,
  1, 0, 1, 0, 0, 0, 0, 0, 0,
  1, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 1, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 1, 1, 0,
  0, 0, 0, 0, 0, 1, 0, 1, 0,
  1, 0, 0, 0, 0, 1, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0
), byrow = TRUE, ncol = 9)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

simplicial_complexes(A)
#>   a-b-c f-g-h a-h d-e d-f i
#> a     1     0   1   0   0 0
#> b     1     0   0   0   0 0
#> c     1     0   0   0   0 0
#> d     0     0   0   1   1 0
#> e     0     0   0   1   0 0
#> f     0     1   0   0   1 0
#> g     0     1   0   0   0 0
#> h     0     1   1   0   0 0
#> i     0     0   0   0   0 1
simplicial_complexes(A, zero_simplex = FALSE)
#>   a-b-c f-g-h a-h d-e d-f
#> a     1     0   1   0   0
#> b     1     0   0   0   0
#> c     1     0   0   0   0
#> d     0     0   0   1   1
#> e     0     0   0   1   0
#> f     0     1   0   0   1
#> g     0     1   0   0   0
#> h     0     1   1   0   0
#> i     0     0   0   0   0
simplicial_complexes(A, complex = "neighbourhood", closed = TRUE)
#>   N[a] N[b] N[c] N[d] N[e] N[f] N[g] N[h] N[i]
#> a    1    1    1    0    0    0    0    1    0
#> b    1    1    1    0    0    0    0    0    0
#> c    1    1    1    0    0    0    0    0    0
#> d    0    0    0    1    1    1    0    0    0
#> e    0    0    0    1    1    0    0    0    0
#> f    0    0    0    1    0    1    1    1    0
#> g    0    0    0    0    0    1    1    1    0
#> h    1    0    0    0    0    1    1    1    0
#> i    0    0    0    0    0    0    0    0    1
simplicial_complexes(A, projection = TRUE, valued = TRUE)$projection2
#>   a b c d e f g h i
#> a 0 1 1 0 0 0 0 1 0
#> b 1 0 1 0 0 0 0 0 0
#> c 1 1 0 0 0 0 0 0 0
#> d 0 0 0 0 1 1 0 0 0
#> e 0 0 0 1 0 0 0 0 0
#> f 0 0 0 1 0 0 1 1 0
#> g 0 0 0 0 0 1 0 1 0
#> h 1 0 0 0 0 1 1 0 0
#> i 0 0 0 0 0 0 0 0 0
```
