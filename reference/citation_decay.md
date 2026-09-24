# Temporal decay weighting for citation edges

Applies an exponential temporal decay to the arcs of a citation
adjacency matrix and optionally normalises the result so that each
citing paper distributes exactly one unit of citation influence among
its references.

## Usage

``` r
citation_decay(A, years, lambda = 0.2, normalize = TRUE)
```

## Arguments

- A:

  A square, named, directed adjacency matrix.

- years:

  Named numeric vector of publication years aligned with row/column
  names of `A`.

- lambda:

  Positive numeric decay rate. Default `0.2`.

- normalize:

  Logical; if `TRUE` (default) each column is divided by its sum so that
  the references of each citing paper sum to one.

## Value

A numeric matrix of the same dimensions as `A` containing the
decay-weighted (and optionally normalised) citation arc weights.
[`traversal_weights`](https://anespinosa.github.io/netmem/reference/traversal_weights.md)
uses only whether each arc is present, so these weights do not change
the search path counts; they can be combined with them, for instance by
multiplying the two matrices element by element.

## Details

As in
[`traversal_weights`](https://anespinosa.github.io/netmem/reference/traversal_weights.md),
`A[i,j] > 0` means that paper `j` cites paper `i`. The weight of the arc
\\i \to j\\ is \\\exp(-\lambda \cdot (y_j - y_i))\\ where \\y_i\\ and
\\y_j\\ are the publication years of \\i\\ and \\j\\ respectively. A
larger \\\lambda\\ discounts older citations more aggressively. The
normalisation divides each column, which holds the references of a
citing paper, by its sum, so that differences in the length of the
reference lists do not inflate the raw weights.

## References

Hummon, N.P. and Doreian, P. (1989). Connectivity in a citation network:
The development of DNA theory. Social Networks. 11(1): 39-63.
[doi:10.1016/0378-8733(89)90017-8](https://doi.org/10.1016/0378-8733%2889%2990017-8)
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
years <- c(P1 = 2000, P2 = 2005, P3 = 2006, P4 = 2010)

citation_decay(A, years, lambda = 0.2)
#>    P1 P2 P3       P4
#> P1  0  1  1 0.000000
#> P2  0  0  0 0.450166
#> P3  0  0  0 0.549834
#> P4  0  0  0 0.000000
```
