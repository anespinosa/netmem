# Traversal weights for main path analysis

Computes the Search Path Count (SPC), the Search Path Link Count (SPLC),
or the Search Path Node Pair (SPNP) of each arc of a citation network
(Hummon and Doreian, 1989; Batagelj, 2003), and the weighted in-degree
and out-degree of each node (Kuan, 2020).

## Usage

``` r
traversal_weights(
  A,
  method = c("spc", "splc", "spnp"),
  years = NULL,
  cutoff = NULL,
  normalized = FALSE
)
```

## Arguments

- A:

  A square, named, directed adjacency matrix in which `A[i,j] > 0` means
  that knowledge flows from paper `i` to paper `j` (paper `j` cites
  paper `i`). Only whether each arc is present is used. See
  [`dag`](https://anespinosa.github.io/netmem/reference/dag.md).

- method:

  One of `"spc"` (default), `"splc"`, or `"spnp"`.

- years:

  Optional named numeric vector of publication years aligned with
  row/column names of `A`. Required when `cutoff` is supplied.

- cutoff:

  Optional integer year. When provided together with `years`, outgoing
  arcs from papers published after `cutoff` are excluded from path
  counting, as a correction for right censoring: the most recent papers
  have not had time to be cited. Such papers become sinks and their
  outgoing arcs get a weight of zero.

- normalized:

  Whether to divide the weights by the total number of search paths of
  the method, so that the weight of an arc is the proportion of the
  search paths that go through it. The total is the number of paths from
  the sources to the sinks (SPC), from any node to the sinks (SPLC), or
  between any two nodes (SPNP), counting only paths with at least one
  arc.

## Value

A named list:

- `edge_weights`:

  Square matrix of the same dimensions as `A` containing the chosen
  traversal weight for each arc.

- `weighted_indegree`:

  Named numeric vector with the sum of the weights of the incoming arcs
  of each node (WiD).

- `weighted_outdegree`:

  Named numeric vector with the sum of the weights of the outgoing arcs
  of each node (WoD).

- `log_forward_source`:

  Log of the number of paths from the sources to each node.

- `log_forward_all`:

  Log of the number of paths from any node to each node, counting the
  node itself.

- `log_backward_sink`:

  Log of the number of paths from each node to the sinks.

- `log_backward_all`:

  Log of the number of paths from each node to any node, counting the
  node itself.

- `sources`:

  Character vector of source node names.

- `sinks`:

  Character vector of sink node names.

- `total_log_paths`:

  Log of the total number of search paths of the method (see
  `normalized`).

- `method`:

  The method string used.

## Details

The weight of an arc is the number of search paths that go through it.
The three weights differ in where the search paths start and end (Liu,
Lu and Ho, 2019):

- SPC:

  Paths from any source to any sink. The weight of the arc \\u \to v\\
  is the number of paths from the sources to \\u\\ times the number of
  paths from \\v\\ to the sinks.

- SPLC:

  Paths from any node to any sink, so that the intermediate papers are
  also origins of knowledge. The first factor becomes the number of
  paths from any node to \\u\\, counting \\u\\ itself.

- SPNP:

  Paths from any node to any node, so that the intermediate papers are
  also origins and destinations of knowledge. The second factor becomes
  the number of paths from \\v\\ to any node, counting \\v\\ itself.

The three weights satisfy SPNP \\\ge\\ SPLC \\\ge\\ SPC. Liu, Lu and Ho
(2019) recommend SPLC to trace the diffusion of knowledge, and Kuan
(2020) argues for SPLC or SPNP.

The weighted in-degree (WiD) and out-degree (WoD) of a node are the sums
of the weights of its incoming and outgoing arcs. Kuan (2020) recommends
the WoD of SPLC or SPNP as the weight of a paper: it counts the
influence of all the papers that precede it, and of the paper itself, on
the papers that follow it, and it is zero for the sinks, whose
importance is yet to be determined. The average of the two, which Kuan
calls WxD, is `(weighted_indegree + weighted_outdegree) / 2`.

The numbers of paths grow exponentially with the length of the chains of
citations, so they are computed in logs to avoid overflow.

## References

Batagelj, V. (2003). Efficient algorithms for citation network analysis.
arXiv:cs/0309023.

Hummon, N.P. and Doreian, P. (1989). Connectivity in a citation network:
The development of DNA theory. Social Networks. 11(1): 39-63.
[doi:10.1016/0378-8733(89)90017-8](https://doi.org/10.1016/0378-8733%2889%2990017-8)
.

Kuan, C. H. (2020). Regarding weight assignment algorithms of main path
analysis and the conversion of arc weights to node weights.
Scientometrics, 124(1), 775-782.
[doi:10.1007/s11192-020-03468-8](https://doi.org/10.1007/s11192-020-03468-8)
.

Liu, J.S. and Lu, L.Y.Y. (2012). An integrated approach for main path
analysis: Development of the Hirsch index as an example. Journal of the
American Society for Information Science and Technology. 63(3): 528-542.
[doi:10.1002/asi.21692](https://doi.org/10.1002/asi.21692) .

Liu, J.S., Lu, L.Y.Y. and Ho, M.H.C. (2019). A few notes on main path
analysis. Scientometrics. 119(1): 379-391.
[doi:10.1007/s11192-019-03034-x](https://doi.org/10.1007/s11192-019-03034-x)
.

Verspagen, B. (2007). Mapping technological trajectories as patent
citation networks: A study on the history of fuel cell research.
Advances in Complex Systems. 10(1): 93-115.
[doi:10.1142/S0219525907000945](https://doi.org/10.1142/S0219525907000945)
.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
# Network of Fig. 2 in Kuan (2020)
A <- matrix(c(
 0,0,0,1,0,0,0,0,0,0,0,0,0,
 0,0,0,1,0,0,0,0,0,0,0,0,0,
 0,0,0,1,0,0,0,0,0,0,0,0,0,
 0,0,0,0,1,1,0,0,0,0,0,0,0,
 0,0,0,0,0,0,1,0,0,0,0,0,0,
 0,0,0,0,0,0,0,1,0,0,0,0,0,
 0,0,0,0,0,0,0,1,1,1,1,0,0,
 0,0,0,0,0,0,0,0,0,0,0,1,1,
 0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0,
 0,0,0,0,0,0,0,0,0,0,0,0,0
), nrow = 13, byrow = TRUE)

rownames(A) <- colnames(A) <- 1:13

spc <- traversal_weights(A, method = "spc")
splc <- traversal_weights(A, method = "splc")
spnp <- traversal_weights(A, method = "spnp")

# Arc 8 -> 12 has SPC 6 and SPLC 12; arc 1 -> 4 has SPNP 13 (Kuan, 2020)
spc$edge_weights["8", "12"]
#> [1] 6
splc$edge_weights["8", "12"]
#> [1] 12
spnp$edge_weights["1", "4"]
#> [1] 13

# Table 4 in Kuan (2020)
cbind(
  SPLC_WoD = splc$weighted_outdegree,
  SPNP_WoD = spnp$weighted_outdegree,
  SPNP_WxD = (spnp$weighted_indegree + spnp$weighted_outdegree) / 2
)
#>    SPLC_WoD SPNP_WoD SPNP_WxD
#> 1         7       13      6.5
#> 2         7       13      6.5
#> 3         7       13      6.5
#> 4        28       48     43.5
#> 5        25       35     33.5
#> 6        10       15     15.5
#> 7        30       36     35.5
#> 8        24       24     28.5
#> 9         0        0      3.0
#> 10        0        0      3.0
#> 11        0        0      3.0
#> 12        0        0      6.0
#> 13        0        0      6.0
```
