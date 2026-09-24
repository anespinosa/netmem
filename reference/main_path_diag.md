# Diagnostics for main path analysis

Computes K-sensitivity of the key-route network and the Jaccard overlap
between SPC- and SPLC-based key routes as a robustness check.

## Usage

``` r
main_path_diag(
  A,
  weights = NULL,
  k_values = c(5L, 10L, 15L, 20L, 30L),
  k_jaccard = 10L
)
```

## Arguments

- A:

  A square, named, directed adjacency matrix in which `A[i,j] > 0` means
  that paper `j` cites paper `i`.

- weights:

  Output of
  [`traversal_weights()`](https://anespinosa.github.io/netmem/reference/traversal_weights.md)
  with `method = "spc"`. Computed internally when `NULL`.

- k_values:

  Integer vector of K values for the sensitivity table. Default
  `c(5, 10, 15, 20, 30)`.

- k_jaccard:

  Integer K used for the SPLC/SPC Jaccard comparison. Default `10L`.

## Value

A named list:

- `k_sensitivity`:

  Data frame with columns `K`, `n_nodes`, `n_edges`, and `new_nodes`
  (marginal nodes added at each K).

- `splc_spc_jaccard`:

  Numeric Jaccard overlap of node sets between SPLC- and SPC-based key
  routes at `k_jaccard`.

- `weight_summary`:

  Summary statistics for nonzero edge weights.

- `n_sources`:

  Number of source nodes.

- `n_sinks`:

  Number of sink nodes.

- `total_log_paths`:

  Log of the total number of search paths of `weights`.

## Details

The K-sensitivity table shows how the size of the key-route network
grows as more seed routes are added. Stabilisation of new-node counts
signals that the main structural backbone has been captured.

The SPLC/SPC Jaccard overlap at `k_jaccard` routes measures whether the
key routes change when the intermediate papers are also counted as
origins of knowledge (SPLC) instead of only the sources (SPC), which is
the main difference between the two weights (Liu, Lu and Ho, 2019). A
value of one means that both weights give the same papers.

## References

Liu, J.S., Lu, L.Y.Y. and Ho, M.H.C. (2019). A few notes on main path
analysis. Scientometrics. 119(1): 379-391.
[doi:10.1007/s11192-019-03034-x](https://doi.org/10.1007/s11192-019-03034-x)
.

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

main_path_diag(A, k_values = c(1L, 2L, 3L))
#> $k_sensitivity
#>   K n_nodes n_edges new_nodes
#> 1 1       3       2        NA
#> 2 2       5       4         2
#> 3 3       6       6         1
#> 
#> $splc_spc_jaccard
#> [1] 1
#> 
#> $weight_summary
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>   1.000   1.000   1.000   1.333   1.750   2.000 
#> 
#> $n_sources
#> [1] 1
#> 
#> $n_sinks
#> [1] 2
#> 
#> $total_log_paths
#> [1] 1.098612
#> 
```
