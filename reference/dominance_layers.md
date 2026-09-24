# Dominance layers

Layers, status and transitive reduction of a strict dominance relation.

## Usage

``` r
dominance_layers(D, reduction = FALSE, direction = c("dominated", "dominates"))
```

## Arguments

- D:

  A binary dominance matrix without cycles, such as the output of
  [`pareto_dominance()`](https://anespinosa.github.io/netmem/reference/pareto_dominance.md)

- reduction:

  Whether to return the transitive reduction of `D`

- direction:

  Whether `D[u, v] = 1` means that `u` is `dominated` by `v` (default)
  or that `u` `dominates` `v`

## Value

This function returns the layers, the layer, status and net dominance of
each node and, if requested, the transitive reduction (with the same
direction as `D`).

## Details

The first layer contains the nodes that are not dominated by any other
node (maximal elements). These nodes are removed, and the procedure is
repeated until all nodes are assigned. Each layer is an antichain, i.e.
the nodes within a layer do not dominate each other.

The maximal elements are of two kinds (Espinosa-Rada, 2026): `dominant`
nodes dominate at least one other node, while `independent` nodes
neither dominate nor are dominated. The remaining nodes are `dominated`.
The net dominance is the number of nodes dominated minus the number of
nodes dominating.

The transitive reduction removes the tie \\u \to v\\ when there is
another node \\k\\ such that \\u \to k \to v\\, which is the usual
representation of a hierarchy (Hasse diagram).

## References

Brandes, U. (2016). Network positions. Methodological Innovations, 9,
1–19.
[doi:10.1177/2059799116630650](https://doi.org/10.1177/2059799116630650)

Espinosa-Rada, A. (2026). Network positions within scholars and
intellectual networks. Journal of Informetrics, 20, 101854.
[doi:10.1016/j.joi.2026.101854](https://doi.org/10.1016/j.joi.2026.101854)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
X <- matrix(c(
  1, 1, 1, 0,
  1, 1, 0, 0,
  1, 0, 0, 0,
  0, 1, 0, 0,
  0, 0, 1, 1
), byrow = TRUE, ncol = 4)
rownames(X) <- c("a1", "a2", "a3", "a4", "a5")
colnames(X) <- c("w1", "w2", "w3", "w4")

D <- pareto_dominance(list(set_inclusion(X)))
dominance_layers(D, reduction = TRUE)
#> $layers
#> $layers[[1]]
#> [1] "a1" "a5"
#> 
#> $layers[[2]]
#> [1] "a2"
#> 
#> $layers[[3]]
#> [1] "a3" "a4"
#> 
#> 
#> $layer_id
#> a1 a2 a3 a4 a5 
#>  1  2  3  3  1 
#> 
#> $status
#>            a1            a2            a3            a4            a5 
#>    "dominant"   "dominated"   "dominated"   "dominated" "independent" 
#> 
#> $net_dominance
#> a1 a2 a3 a4 a5 
#>  3  1 -2 -2  0 
#> 
#> $reduction
#>    a1 a2 a3 a4 a5
#> a1  0  0  0  0  0
#> a2  1  0  0  0  0
#> a3  0  1  0  0  0
#> a4  0  1  0  0  0
#> a5  0  0  0  0  0
#> 
```
