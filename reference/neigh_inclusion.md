# Neighbourhood inclusion

Neighbourhood-inclusion preorder of an undirected network (Brandes,
2016; Schoch and Brandes, 2016).

## Usage

``` r
neigh_inclusion(A, closed = TRUE, direction = c("dominated", "dominates"))
```

## Arguments

- A:

  A symmetric matrix object

- closed:

  Whether the neighbourhood of the dominating node is closed (i.e.
  includes the node itself)

- direction:

  Whether `P[u, v] = 1` means that `u` is `dominated` by `v` (default,
  as in Schoch and Brandes, 2016) or that `u` `dominates` `v`

## Value

This function returns a binary matrix `P` of the neighbourhood
inclusion, where `P[u, v] = 1` when the neighbours of `u` are also
neighbours of `v`, in the `direction` asked for.

## Details

A node \\u\\ is dominated by a node \\v\\ if the open neighbourhood of
\\u\\ is included in the closed neighbourhood of \\v\\, \\N(u) \subseteq
N\[v\]\\. With `closed = FALSE` both neighbourhoods are open, \\N(u)
\subseteq N(v)\\.

In matrix form, \\u\\ is dominated by \\v\\ when the number of shared
neighbours equals the degree of \\u\\.

## References

Brandes, U. (2016). Network positions. Methodological Innovations, 9,
1–19.
[doi:10.1177/2059799116630650](https://doi.org/10.1177/2059799116630650)

Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in
social networks. European Journal of Applied Mathematics, 27(6),
971–985.
[doi:10.1017/S0956792516000401](https://doi.org/10.1017/S0956792516000401)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 1, 0, 0, 0, 0, 0,
  1, 0, 1, 1, 1, 0, 0, 0, 0,
  1, 1, 0, 1, 0, 1, 0, 0, 0,
  1, 1, 1, 0, 1, 1, 0, 0, 0,
  0, 1, 0, 1, 0, 1, 1, 0, 0,
  0, 0, 1, 1, 1, 0, 1, 0, 0,
  0, 0, 0, 0, 1, 1, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 1, 0
), byrow = TRUE, ncol = 9)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

neigh_inclusion(A)
#>   a b c d e f g h i
#> a 0 1 1 1 0 0 0 0 0
#> b 0 0 0 1 0 0 0 0 0
#> c 0 0 0 1 0 0 0 0 0
#> d 0 0 0 0 0 0 0 0 0
#> e 0 0 0 0 0 0 0 0 0
#> f 0 0 0 0 0 0 0 0 0
#> g 0 0 0 0 0 0 0 0 0
#> h 0 0 0 0 0 0 0 0 0
#> i 0 0 0 0 0 0 1 1 0
```
