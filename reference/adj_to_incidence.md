# Convert an Adjacency Matrix to an Incidence Matrix

This function transforms an adjacency matrix into an incidence matrix.

## Usage

``` r
adj_to_incidence(A, loops = TRUE, directed = TRUE, weighted = TRUE)
```

## Arguments

- A:

  A square numeric matrix representing the adjacency matrix of a graph.
  The matrix should have non-negative values, where \`A\[i, j\]\`
  represents the weight of the edge from node \`i\` to node \`j\`.

- loops:

  Logical. If \`TRUE\`, self-loops (edges from a node to itself) are
  included in the incidence matrix. If \`FALSE\`, they are removed.
  Default is \`TRUE\`.

- directed:

  Logical. If \`TRUE\`, the graph is treated as directed, meaning each
  edge has a specific source and target. If \`FALSE\`, the graph is
  treated as undirected, and edges are symmetrically represented.
  Default is \`TRUE\`.

- weighted:

  Logical. If \`TRUE\`, edge weights from \`A\` are included in the
  incidence matrix. If \`FALSE\`, all edges are treated as having weight
  \`1\`. Default is \`TRUE\`.

## Value

A numeric matrix where rows represent nodes and columns represent
edges. - In a \*\*directed\*\* network, a source node has a negative
value (-weight), and a target node has a positive value (+weight). - In
an \*\*undirected\*\* network, both nodes involved in an edge share the
weight (positive values). - If \`weighted = FALSE\`, all edges have a
weight of \`1\`.

## Examples

``` r
# Define an adjacency matrix (directed and weighted)
A <- matrix(c(
  1, 3, 0, 0, 2,
  0, 0, 2, 0, 0,
  5, 0, 0, 0, 0,
  0, 0, 0, 0, 1,
  0, 4, 0, 0, 0
), byrow = TRUE, nrow = 5)

# Convert to an incidence matrix (directed, weighted)
(inc_matrix <- adj_to_incidence(A))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#> [1,]    1    5   -3    0    0   -2    0
#> [2,]    0    0    3    4   -2    0    0
#> [3,]    0   -5    0    0    2    0    0
#> [4,]    0    0    0    0    0    0   -1
#> [5,]    0    0    0   -4    0    2    1

# Undirected, weighted graph
(inc_matrix_undirected <- adj_to_incidence(A, directed = FALSE))
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    3    0    2    0
#> [2,]    0    3    2    0    0
#> [3,]    0    0    2    0    0
#> [4,]    0    0    0    0    1
#> [5,]    0    0    0    2    1

# Directed, unweighted graph
(inc_matrix_unweighted <- adj_to_incidence(A, weighted = FALSE))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#> [1,]    1    1   -1    0    0   -1    0
#> [2,]    0    0    1    1   -1    0    0
#> [3,]    0   -1    0    0    1    0    0
#> [4,]    0    0    0    0    0    0   -1
#> [5,]    0    0    0   -1    0    1    1

# Ignore loops
(inc_matrix_no_loops <- adj_to_incidence(A, loops = FALSE))
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    5   -3    0    0   -2    0
#> [2,]    0    3    4   -2    0    0
#> [3,]   -5    0    0    2    0    0
#> [4,]    0    0    0    0    0   -1
#> [5,]    0    0   -4    0    2    1
```
