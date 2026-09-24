# Hyper-event dominance

Dominance among authors based on the hyper-event chain Author -\> Citing
paper -\> Cited paper -\> Author (Espinosa-Rada, 2026).

## Usage

``` r
hyperevent_dominance(
  X,
  W,
  Xb = X,
  tau = 2,
  dimensions = c("authored", "cited_papers", "cited_authors"),
  closed = c(FALSE, TRUE, TRUE),
  strict = c("asymmetric", "proper"),
  closure_papers = c("citing", "authored"),
  max_authors = NULL,
  team_size = NULL,
  direction = c("dominated", "dominates")
)
```

## Arguments

- X:

  An incidence matrix of authors (rows) and the citing papers they
  authored (columns)

- W:

  A square citation matrix where `W[p, q] = 1` if paper `p` cites paper
  `q`

- Xb:

  An incidence matrix of authors and the cited papers they authored. By
  default, the same as `X`

- tau:

  Minimum number of dimensions in which an author should be weakly
  dominated

- dimensions:

  The dimensions to be considered: `authored`, `cited_papers` and/or
  `cited_authors`

- closed:

  A logical vector with whether the neighbourhoods of `authored`,
  `cited_papers` and `cited_authors` are closed

- strict:

  Whether the strict dominance is `asymmetric` (default) or a `proper`
  inclusion

- closure_papers:

  Whether the closed neighbourhood of cited papers adds the `citing`
  papers of the author (default) or every paper `authored`

- max_authors:

  If not NULL, the citing papers with more authors than this number are
  excluded

- team_size:

  Number of authors of each paper, in the same order as the columns of
  `X`. By default, the column sums of `X`

- direction:

  Whether `D[u, v] = 1` means that `u` is `dominated` by `v` (default)
  or that `u` `dominates` `v`, as in the figures of Espinosa-Rada (2026)

## Value

This function returns a binary dominance matrix `D` of the authors.

## Details

Each author has three neighbourhoods built from the hyper-events in
which the author wrote the citing paper:

(`authored`) productive participation: the citing papers authored,
\\X\\,

(`cited_papers`) citation reach: the cited papers, \\X \circ W\\. Its
closed version adds the papers authored,

(`cited_authors`) recognition: the cited authors, \\X \circ W \circ
X_b^T\\. Its closed version adds the author itself.

Author \\a_i\\ dominates \\a_j\\ when the neighbourhood of \\a_j\\ is
included in the (closed) neighbourhood of \\a_i\\ in at least `tau`
dimensions, and the dominance is strict in at least one dimension (see
[`pareto_dominance()`](https://anespinosa.github.io/netmem/reference/pareto_dominance.md)).
Authors without hyper-events are excluded.

The defaults reproduce the analysis of Espinosa-Rada (2026): the three
dimensions, open neighbourhoods for the authored papers and closed
neighbourhoods for the cited papers and authors, and asymmetric strict
dominance. The alternatives are:

`strict = "proper"` follows the formal definition of strict dominance as
proper inclusion, \\N_k(a_j) \subsetneq N^\*\_k(a_i)\\. As closed
neighbourhoods add elements to \\a_i\\, proper inclusion is less
demanding than the asymmetric criterion, where \\a_i\\ should not be
included in \\a_j\\. With proper inclusion, two authors might dominate
each other, so the relation is not always a partial order. The same
might happen with the asymmetric criterion when `tau = 1`, if each
author dominates the other in a different dimension.

`closure_papers` sets which papers of \\a_i\\ are added to close the
neighbourhood of cited papers. With `"citing"` (default, as in the
analysis scripts of the article) they are the citing papers of \\a_i\\
with hyper-events. With `"authored"` they are every paper authored by
\\a_i\\, in `X` or in `Xb`, as in the text of Section 3.5, so that a
paper of \\a_i\\ cited by \\a_j\\ is in the closed neighbourhood of
\\a_i\\ even when it does not cite other papers of the corpus.

`max_authors` excludes the citing papers of large teams before computing
the neighbourhoods, as a robustness check for consortium papers (in the
article, papers with more than 20 authors). As the matrices might only
contain some of the authors of each paper (e.g. a bounded population),
the number of authors can be given in `team_size`.

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
rownames(X) <- c("a1", "a2", "a3", "a4", "a5")
colnames(X) <- c("w1", "w2", "w3", "w4")

W <- matrix(c(
  0, 1, 1, 0,
  0, 0, 1, 0,
  0, 0, 0, 1,
  0, 0, 0, 0
), byrow = TRUE, ncol = 4)
rownames(W) <- colnames(X)
colnames(W) <- colnames(X)

hyperevent_dominance(X, W, tau = 2)
#>    a1 a2 a3 a4 a5
#> a1  0  0  0  0  0
#> a2  1  0  0  0  0
#> a3  1  1  0  0  0
#> a4  1  1  1  0  0
#> a5  1  0  0  0  0
hyperevent_dominance(X, W, tau = 2, strict = "proper")
#>    a1 a2 a3 a4 a5
#> a1  0  0  0  0  0
#> a2  1  0  1  0  0
#> a3  1  1  0  0  0
#> a4  1  1  1  0  0
#> a5  1  0  0  0  0
hyperevent_dominance(X, W, tau = 1, dimensions = c("authored", "cited_authors"))
#>    a1 a2 a3 a4 a5
#> a1  0  0  0  0  0
#> a2  1  0  0  0  0
#> a3  1  1  0  0  0
#> a4  1  1  1  0  0
#> a5  1  1  1  1  0
```
