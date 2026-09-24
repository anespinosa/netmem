# Neighbourhood inclusion in directed networks

Neighbourhood-inclusion preorders for directed networks (Marmulla and
Brandes, 2026), which extend the vicinal preorder of the undirected case
to the criteria that different families of centrality indices preserve.

## Usage

``` r
dir_inclusion(
  A,
  type = c("radial_out", "radial_in", "hierarchical_down", "hierarchical_up", "medial"),
  strength = c("strong", "weak"),
  direction = c("dominated", "dominates")
)
```

## Arguments

- A:

  A square matrix

- type:

  The criterion: `radial_out` (default), `radial_in`,
  `hierarchical_down`, `hierarchical_up` or `medial`

- strength:

  Whether the neighbourhoods are open (`strong`, default) or closed
  (`weak`). It is ignored for the medial criterion

- direction:

  Whether `P[u, v] = 1` means that `u` is `dominated` by `v` (default)
  or that `u` `dominates` `v`

## Value

This function returns a binary matrix `P` of the criterion asked for in
`type`, where `P[u, v] = 1` when the neighbourhoods of `u` are included
in those of `v`, in the `direction` asked for.

## Details

Let \\N^+(i)\\ be the nodes that \\i\\ sends ties to, \\N^-(i)\\ the
ones that send ties to \\i\\, and \\N\[i\]\\ the same set including
\\i\\. The strong relations use the open neighbourhoods and the weak
ones the closed neighbourhoods:

`radial_out`: \\N^+(i) \subseteq N^+(j)\\, preserved by the indices that
measure how far a node reaches, such as out-degree and closeness.

`radial_in`: \\N^-(i) \subseteq N^-(j)\\, the same for the ties
received.

`hierarchical_down`: \\N^+(i) \subseteq N^+(j)\\ and \\N^-(i) \supseteq
N^-(j)\\, so \\j\\ sends more and receives less than \\i\\.

`hierarchical_up`: \\N^-(i) \subseteq N^-(j)\\ and \\N^+(i) \supseteq
N^+(j)\\, so \\j\\ receives more and sends less, which is the criterion
of the indices of status.

`medial`: \\N^+(i) \subseteq N^+\[j\]\\ and \\N^-(i) \subseteq
N^-\[j\]\\, with two extra conditions when \\i\\ and \\j\\ are adjacent,
so that the advantage that the dominated node has from that tie is
compensated. It is the criterion preserved by betweenness.

## References

Marmulla, G. and Brandes, U. (2026). Centrality in directed networks.
Social Networks, 86, 23–34.
[doi:10.1016/j.socnet.2026.01.001](https://doi.org/10.1016/j.socnet.2026.01.001)

Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in
social networks. European Journal of Applied Mathematics, 27(6),
971–985.
[doi:10.1017/S0956792516000401](https://doi.org/10.1017/S0956792516000401)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0,
  0, 0, 1, 0, 0,
  0, 0, 0, 1, 1,
  0, 0, 0, 0, 1,
  0, 0, 0, 0, 0
), byrow = TRUE, ncol = 5)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

dir_inclusion(A, type = "radial_out")
#>   a b c d e
#> a 0 0 0 0 0
#> b 1 0 0 0 0
#> c 0 0 0 0 0
#> d 0 0 1 0 0
#> e 1 1 1 1 0
dir_inclusion(A, type = "medial")
#>   a b c d e
#> a 0 1 0 0 0
#> b 0 0 0 0 0
#> c 0 0 0 0 0
#> d 0 0 0 0 0
#> e 0 0 0 1 0
```
