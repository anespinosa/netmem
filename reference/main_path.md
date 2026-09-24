# Main path extraction from a citation network

Extracts the main path or key-route network from a directed citation
adjacency matrix using traversal weights (SPC, SPLC, or SPNP).

## Usage

``` r
main_path(
  A,
  weights = NULL,
  method = c("global", "local", "key_route"),
  weight_type = c("spc", "splc", "spnp"),
  k = 1L,
  seeds = NULL
)
```

## Arguments

- A:

  A square, named, directed adjacency matrix in which `A[i,j] > 0` means
  that paper `j` cites paper `i`, so that the paths follow the flow of
  knowledge. See
  [`dag`](https://anespinosa.github.io/netmem/reference/dag.md).

- weights:

  Output of
  [`traversal_weights()`](https://anespinosa.github.io/netmem/reference/traversal_weights.md).
  Computed internally with `weight_type` when `NULL`.

- method:

  One of `"global"` (default), `"local"`, or `"key_route"`.

- weight_type:

  One of `"spc"` (default), `"splc"`, or `"spnp"`. Ignored when
  `weights` is supplied.

- k:

  Integer number of seed routes for `method = "key_route"`. Default
  `1L`.

- seeds:

  Character vector of seed node names for `method = "local"`.

## Value

A named list:

- `nodes`:

  Character vector of node names on the main path or key-route network
  (union across all routes).

- `edges`:

  Square adjacency matrix restricted to path nodes and edges, with the
  same values as `A` for included edges and zero elsewhere.

- `routes`:

  List of character vectors, one per extracted route, each giving the
  ordered sequence of node names.

- `weights`:

  The
  [`traversal_weights()`](https://anespinosa.github.io/netmem/reference/traversal_weights.md)
  result used.

## Details

Three extraction strategies are available:

- `"global"`:

  Finds the single source-to-sink path that maximises the total
  accumulated edge weight, using dynamic programming along the
  topological order.

- `"local"`:

  Traces backward and forward from each node in `seeds`, always
  following the edge with the highest weight. Returns one route per
  seed.

- `"key_route"`:

  Ranks all edges by weight descending; for each of the top `k` seed
  edges not yet covered by a previous route, traces backward from the
  edge's tail and forward from the edge's head. The union of all routes
  forms a sub-DAG capturing multiple intellectual trajectories (Liu &
  Lu, 2012).

## References

Hummon, N.P. and Doreian, P. (1989). Connectivity in a citation network:
The development of DNA theory. Social Networks. 11(1): 39-63.
[doi:10.1016/0378-8733(89)90017-8](https://doi.org/10.1016/0378-8733%2889%2990017-8)
.

Liu, J.S. and Lu, L.Y.Y. (2012). An integrated approach for main path
analysis: Development of the Hirsch index as an example. Journal of the
American Society for Information Science and Technology. 63(3): 528-542.
[doi:10.1002/asi.21692](https://doi.org/10.1002/asi.21692) .

Lucio-Arias, D. and Leydesdorff, L. (2008). Main-path analysis and
path-dependent transitions in HistCite-based historiographs. Journal of
the American Society for Information Science and Technology. 59(12):
1948-1962. [doi:10.1002/asi.20903](https://doi.org/10.1002/asi.20903) .

Verspagen, B. (2007). Mapping technological trajectories as patent
citation networks. Advances in Complex Systems. 10(1): 93-115.
[doi:10.1142/S0219525907000945](https://doi.org/10.1142/S0219525907000945)
.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  0, 0, 0, 1, 1, 0,
  0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0
), byrow = TRUE, nrow = 6)
rownames(A) <- letters[1:6]
colnames(A) <- letters[1:6]

mp <- main_path(A, method = "global")
mp$routes
#> [[1]]
#> [1] "a" "b" "e" "f"
#> 

kr <- main_path(A, method = "key_route", k = 2L)
kr$nodes
#> [1] "a" "b" "d" "e" "f"
```
