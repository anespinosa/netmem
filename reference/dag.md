# DAG validation and topological ordering for citation networks

`dag_check` verifies whether a directed adjacency matrix represents a
directed acyclic graph (DAG) and, when it does not, removes a set of
arcs that breaks every cycle. `dag_sort` returns the node names in
topological order (sources first, sinks last).

## Usage

``` r
dag_check(A)

dag_sort(A)
```

## Arguments

- A:

  A square, named, directed adjacency matrix.

## Value

`dag_check` returns a named list:

- `is_dag`:

  Logical; `TRUE` if the original matrix was already acyclic.

- `n_removed`:

  Integer number of arcs removed, self-citations included.

- `A`:

  Adjacency matrix with the removed arcs set to zero.

`dag_sort` returns a character vector of node names in topological
order.

## Details

In the convention used throughout the main path functions, `A[i,j] > 0`
means that knowledge flows from paper `i` to paper `j`, that is, paper
`j` cites paper `i` (Liu and Lu, 2012; Kuan, 2020). Sources (in-degree
zero) are the papers that cite nobody in the corpus; sinks (out-degree
zero) are the papers that nobody in the corpus cites. A matrix in which
`A[i,j] > 0` means that `i` cites `j` should be transposed first with
`t(A)`.

The order is obtained with the algorithm of Kahn (1962): the sources are
placed first, their arcs are removed, and the nodes left without
incoming arcs are placed next, until no node remains. When some nodes
are never left without incoming arcs, they lie on a cycle.

A citation network can have cycles, for instance when two papers
published at the same time cite each other, and a paper citing itself is
a cycle of length one. `dag_check` removes the self-citations and then
the arcs that point backwards in the ordering of Eades, Lin and Smyth
(1993). Arcs that can be restored without closing a cycle are restored,
so that no removed arc is unnecessary. The set is small but is not
guaranteed to be the smallest possible, which is a hard problem. Liu, Lu
and Ho (2019) discuss alternatives that keep all the citations, such as
merging the papers of a cycle into one node, which should be applied
before this function when the removed arcs matter.

## References

Eades, P., Lin, X. and Smyth, W.F. (1993). A fast and effective
heuristic for the feedback arc set problem. Information Processing
Letters. 47(6): 319-323.
[doi:10.1016/0020-0190(93)90079-O](https://doi.org/10.1016/0020-0190%2893%2990079-O)
.

Hummon, N.P. and Doreian, P. (1989). Connectivity in a citation network:
The development of DNA theory. Social Networks. 11(1): 39-63.
[doi:10.1016/0378-8733(89)90017-8](https://doi.org/10.1016/0378-8733%2889%2990017-8)
.

Kahn, A.B. (1962). Topological sorting of large networks. Communications
of the ACM. 5(11): 558-562.
[doi:10.1145/368996.369025](https://doi.org/10.1145/368996.369025) .

Liu, J.S., Lu, L.Y.Y. and Ho, M.H.C. (2019). A few notes on main path
analysis. Scientometrics. 119(1): 379-391.
[doi:10.1007/s11192-019-03034-x](https://doi.org/10.1007/s11192-019-03034-x)
.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
# P1 is cited by P2 and P3, which are both cited by P4
A <- matrix(c(
  0, 1, 1, 0,
  0, 0, 0, 1,
  0, 0, 0, 1,
  0, 0, 0, 0
), byrow = TRUE, nrow = 4)
rownames(A) <- c("P1", "P2", "P3", "P4")
colnames(A) <- c("P1", "P2", "P3", "P4")

dag_check(A)
#> $is_dag
#> [1] TRUE
#> 
#> $n_removed
#> [1] 0
#> 
#> $A
#>    P1 P2 P3 P4
#> P1  0  1  1  0
#> P2  0  0  0  1
#> P3  0  0  0  1
#> P4  0  0  0  0
#> 

# P4 is also cited by P1, which closes a cycle
A_cycle <- A
A_cycle["P4", "P1"] <- 1
dag_check(A_cycle)
#> Warning: 1 arc(s) removed to enforce the DAG structure
#> $is_dag
#> [1] FALSE
#> 
#> $n_removed
#> [1] 1
#> 
#> $A
#>    P1 P2 P3 P4
#> P1  0  1  1  0
#> P2  0  0  0  1
#> P3  0  0  0  1
#> P4  0  0  0  0
#> 
dag_sort(A)
#> [1] "P1" "P2" "P3" "P4"
```
