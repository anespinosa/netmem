# Krackhardt's dimensions of informal organisations

The four dimensions that Krackhardt (1994) uses to compare a network
with a perfect hierarchy (an out-tree): connectedness, hierarchy,
efficiency and least upper boundedness.

## Usage

``` r
krackhardt_index(A, lubness = c("upper", "least"))
```

## Arguments

- A:

  A square matrix

- lubness:

  Whether every pair of nodes should have an `upper` bound (default,
  Everett and Krackhardt, 2012) or a `least` upper bound (Krackhardt,
  1994)

## Value

This function returns the connectedness, hierarchy, efficiency and least
upper boundedness of the network.

## Details

The measures are computed on the reachability matrix \\R\\, where
\\R\[i,j\] = 1\\ when \\j\\ can be reached from \\i\\:

`connectedness` is the proportion of pairs of nodes that are connected
in the underlying graph, i.e. one minus the proportion of pairs in
different weak components.

`hierarchy` is one minus the proportion of the reachable ordered pairs
that are also reachable in the opposite direction. It is one when no
pair of nodes can reach each other.

`efficiency` is one minus the proportion of the ties that are not needed
to keep the same weak components. A network is efficient when it has no
more ties than a spanning tree.

`lubness` (upper boundedness) is the proportion of the pairs of nodes
that have an upper bound, i.e. a node that reaches both of them. Everett
and Krackhardt (2012) recommend this version, as the original condition
asks for a *least* upper bound, an upper bound that is on a directed
path from every other upper bound to both nodes, which need not be
unique and can be a very distant node. The original condition is used
with `lubness = "least"`. In both cases a node reaches itself, and the
violations are counted within the weak components of more than two
nodes.

All the measures are one for a perfect out-tree.

## References

Everett, M. G. and Krackhardt, D. (2012). A second look at Krackhardt's
graph theoretical dimensions of informal organizations. Social Networks,
34(2), 159–163.
[doi:10.1016/j.socnet.2011.10.006](https://doi.org/10.1016/j.socnet.2011.10.006)

Krackhardt, D. (1994). Graph theoretical dimensions of informal
organizations. In K. M. Carley and M. J. Prietula (Eds.), Computational
Organization Theory (pp. 89–111). Hillsdale, NJ: Lawrence Erlbaum.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
# A perfect out-tree
A <- matrix(c(
  0, 1, 1, 0, 0, 0, 0,
  0, 0, 0, 1, 1, 0, 0,
  0, 0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0
), byrow = TRUE, ncol = 7)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

krackhardt_index(A)
#> $connectedness
#> [1] 1
#> 
#> $hierarchy
#> [1] 1
#> 
#> $efficiency
#> [1] 1
#> 
#> $lubness
#> [1] 1
#> 
krackhardt_index(A, lubness = "least")
#> $connectedness
#> [1] 1
#> 
#> $hierarchy
#> [1] 1
#> 
#> $efficiency
#> [1] 1
#> 
#> $lubness
#> [1] 1
#> 
```
