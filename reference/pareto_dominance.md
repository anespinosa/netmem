# Pareto dominance

Pareto-style dominance across several neighbourhood-inclusion relations
(Espinosa-Rada, 2026).

## Usage

``` r
pareto_dominance(
  inclusions,
  tau = length(inclusions),
  strict = NULL,
  direction = c("dominated", "dominates")
)
```

## Arguments

- inclusions:

  A list of binary matrices where `[u, v] = 1` if `u` is included in `v`

- tau:

  Minimum number of relations in which `u` should be weakly dominated by
  `v`

- strict:

  An optional list of binary matrices, in the same order as
  `inclusions`, where `[u, v] = 1` if the inclusion of `u` in `v` is
  strict. If NULL, the asymmetric criterion is used

- direction:

  Whether `D[u, v] = 1` means that `u` is `dominated` by `v` (default)
  or that `u` `dominates` `v`

## Value

This function returns a binary dominance matrix `D`.

## Details

Given \\K\\ inclusion matrices (e.g. from
[`set_inclusion()`](https://anespinosa.github.io/netmem/reference/set_inclusion.md)),
\\u\\ is dominated by \\v\\ if (i) the neighbourhood of \\u\\ is
included in the neighbourhood of \\v\\ in at least `tau` relations (weak
dominance), and (ii) the dominance is strict in at least one relation.

By default, the dominance is strict when it is asymmetric: the
neighbourhood of \\u\\ is included in the neighbourhood of \\v\\ but not
the other way around. Other criteria, such as proper inclusion, can be
given in `strict` (see `set_inclusion(proper = TRUE)`).

With `tau = K` every relation should agree (unanimity), with a majority
of relations the dominance is less demanding, and with `tau = 1` one
relation is enough.

## References

Espinosa-Rada, A. (2026). Network positions within scholars and
intellectual networks. Journal of Informetrics, 20, 101854.
[doi:10.1016/j.joi.2026.101854](https://doi.org/10.1016/j.joi.2026.101854)

Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in
social networks. European Journal of Applied Mathematics, 27(6),
971–985.
[doi:10.1017/S0956792516000401](https://doi.org/10.1017/S0956792516000401)

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
Y <- matrix(c(
  1, 1, 0,
  1, 0, 0,
  1, 0, 0,
  0, 1, 1,
  0, 0, 1
), byrow = TRUE, ncol = 3)
rownames(X) <- c("a1", "a2", "a3", "a4", "a5")
rownames(Y) <- rownames(X)

pareto_dominance(list(set_inclusion(X), set_inclusion(Y)), tau = 2)
#>    a1 a2 a3 a4 a5
#> a1  0  0  0  0  0
#> a2  1  0  0  0  0
#> a3  1  1  0  0  0
#> a4  0  0  0  0  0
#> a5  0  0  0  0  0
```
