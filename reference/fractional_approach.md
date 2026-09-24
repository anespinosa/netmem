# Fractional approach

Citation, co-citation and bibliographic coupling networks with full or
fractional counting (Batagelj, 2020).

## Usage

``` r
fractional_approach(
  A1,
  A2 = NULL,
  approach = c("citation", "cocitation", "bcoupling"),
  fractional = TRUE,
  symmetric = c("none", "average", "minimum", "maximum", "geometric", "harmonic",
    "jaccard")
)
```

## Arguments

- A1:

  A citation network between works, where `A1[p, q] = 1` if the work p
  cites the work q

- A2:

  For `approach = "citation"`, the authorship matrix, with the works in
  rows and the authors in columns

- approach:

  Character string, “citation”, “cocitation” and “bcoupling”

- fractional:

  Whether to use fractional counting (default) or full counting

- symmetric:

  For fractional bibliographic coupling, the symmetric similarity:
  `none` (default, the proportion of shared references), `average`,
  `minimum`, `maximum`, `geometric`, `harmonic` or `jaccard`

## Value

Return the citation network between authors, the co-citation network or
the bibliographic coupling network.

## Details

`A1` is a citation network \\Ci\\ between works, where `A1[p, q] = 1`
when the work \\p\\ cites the work \\q\\. With fractional counting each
work has a total weight of one, which is divided equally among the works
it cites: \\Cin = D \cdot Ci\\, where \\D\\ is the diagonal matrix of
one over the number of references of each work (one when it has none).
Fractional counting prevents the works with many references, such as
reviews, from dominating the result (Batagelj, 2020).

`approach = "cocitation"`: \\Ci^T Ci\\, the number of works that cite
both works, or with fractional counting \\Cin^T Cin\\, in which each
citing work contributes a total of one.

`approach = "bcoupling"`: \\Ci \cdot Ci^T\\, the number of works cited
by both works. Fractional counting cannot be applied in the same way to
bibliographic coupling (Batagelj, 2020: 631), so it gives \\biC = Cin
\cdot Ci^T\\, the proportion of the references of \\p\\ that it shares
with \\q\\, which is not symmetric. `symmetric` turns it into a
symmetric similarity: the `average`, the `minimum`, the `maximum`, the
`geometric` mean (the cosine of Salton), the `harmonic` mean, or the
`jaccard` index, the shared references divided by the references of
either work.

`approach = "citation"`: the citations between the authors of the works,
given the authorship matrix \\WA\\ in `A2` (works in rows, authors in
columns): \\WA^T \cdot Ci \cdot WA\\, the number of times the works of
an author cite the works of another. With fractional counting each work
is divided equally among its authors, \\WAn^T \cdot Ci \cdot WAn\\, so
that the total of the network is the number of citations.

Which count should be conserved guides the choice between the two
protocols (Prathap and Mukherjee, 2020): full counting conserves the
number of paths between the nodes, and fractional counting the number of
nodes (works).

## References

Batagelj, V. (2020). On fractional approach to analysis of linked
networks. Scientometrics, 123(2), 621-633.
[doi:10.1007/s11192-020-03383-y](https://doi.org/10.1007/s11192-020-03383-y)

Batagelj, V. (2022). Analysis of the Southern women network using
fractional approach. Social Networks, 68, 229-236
[doi:10.1016/j.socnet.2021.08.001](https://doi.org/10.1016/j.socnet.2021.08.001)

Batagelj, V., & Cerinšek, M. (2013). On bibliographic networks.
Scientometrics, 96(3), 845–864.
[doi:10.1007/s11192-012-0940-1](https://doi.org/10.1007/s11192-012-0940-1)

Prathap, G., & Mukherjee, S. (2020). Letter to the Editor: Comments on
the paper of Batagelj—on fractional approach to analysis of linked
networks. Scientometrics, 124(3), 2717–2722.
[doi:10.1007/s11192-020-03541-2](https://doi.org/10.1007/s11192-020-03541-2)

## Author

Alejandro Espinosa-Rada

## Examples

``` r

# Five works: w1 cites w2 and w3, w4 cites w2, w3 and w5, w5 cites w3
Ci <- matrix(c(
  0, 1, 1, 0, 0,
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0,
  0, 1, 1, 0, 1,
  0, 0, 1, 0, 0
), byrow = TRUE, ncol = 5)
rownames(Ci) <- colnames(Ci) <- paste0("w", 1:5)

# Authors of the works
WA <- matrix(c(
  1, 1, 0,
  0, 1, 0,
  0, 0, 1,
  1, 0, 0,
  0, 0, 1
), byrow = TRUE, ncol = 3)
rownames(WA) <- rownames(Ci)
colnames(WA) <- c("a1", "a2", "a3")

fractional_approach(Ci, WA, approach = "citation")
#>    a1  a2  a3
#> a1  0 1.5 2.5
#> a2  0 0.5 0.5
#> a3  0 0.0 1.0
fractional_approach(Ci, approach = "cocitation")
#>    w1        w2        w3 w4        w5
#> w1  0 0.0000000 0.0000000  0 0.0000000
#> w2  0 0.3611111 0.3611111  0 0.1111111
#> w3  0 0.3611111 1.3611111  0 0.1111111
#> w4  0 0.0000000 0.0000000  0 0.0000000
#> w5  0 0.1111111 0.1111111  0 0.1111111
fractional_approach(Ci, approach = "bcoupling", symmetric = "geometric")
#>           w1 w2 w3        w4        w5
#> w1 1.0000000  0  0 0.8164966 0.7071068
#> w2 0.0000000  0  0 0.0000000 0.0000000
#> w3 0.0000000  0  0 0.0000000 0.0000000
#> w4 0.8164966  0  0 1.0000000 0.5773503
#> w5 0.7071068  0  0 0.5773503 1.0000000
```
