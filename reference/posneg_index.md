# Positive-negative centrality

Positive-negative centrality

## Usage

``` r
posneg_index(A, select = c("all", "in", "out"))
```

## Source

Adapted from David Schoch 'signnet'

## Arguments

- A:

  A signed symmetric matrix (i.e., with ties that are either -1, 0 or 1)

- select:

  Whether to consider the direction of the outgoing ties. Considering
  `all` (default), `in` or `out` ties.

## Value

This function return the positive-negative centrality index for signed
networks (Everett and Borgatti).

## References

Everett, Martin and Borgatti, Stephen (2014). Networks containing
negative ties. Social Networks, 38, 111-120.
[doi:10.1016/j.socnet.2014.03.005](https://doi.org/10.1016/j.socnet.2014.03.005)

## Examples

``` r
A <- matrix(
  c(
    0, 1, -1, -1, -1, -1, 0, 0, 0, 0, 0, -1, 0, 0, 1, 1,
    1, 0, -1, 0, -1, -1, 0, 0, -1, -1, 0, 0, 0, 0, 1, 1,
    -1, -1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
    -1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
    -1, -1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, -1, -1,
    -1, -1, 1, 0, 0, 0, 1, 1, -1, 0, 1, 1, -1, 0, 0, -1,
    0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0,
    0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, -1, 0, 0,
    0, -1, 0, 0, 1, -1, 0, 0, 0, 1, -1, 0, 1, 0, -1, 0,
    0, -1, 0, 0, 0, 0, 0, 0, 1, 0, -1, 0, 1, 0, -1, 0,
    0, 0, 0, 0, 0, 1, 1, 1, -1, -1, 0, 1, -1, 0, -1, -1,
    -1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, -1, -1, -1,
    0, 0, 0, 0, 0, -1, 1, 0, 1, 1, -1, 0, 0, 1, -1, -1,
    0, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0, -1, 1, 0, 0, -1,
    1, 1, 0, 0, -1, 0, 0, 0, -1, -1, -1, -1, -1, 0, 0, 1,
    1, 1, 0, 0, -1, -1, 0, 0, 0, 0, -1, -1, -1, -1, 1, 0
  ),
  ncol = 16, nrow = 16, byrow = TRUE
)
label <- c(
  "Gavev", "Kotun", "Ove", "Alika", "Nagam", "Gahuk", "Masil", "Ukudz",
  "Notoh", "Kohik", "Geham", "Asaro", "Uheto", "Seuve", "Nagad", "Gama"
)
rownames(A) <- label
colnames(A) <- rownames(A)
posneg_index(A, select = c("all"))
#>     Gavev     Kotun       Ove     Alika     Nagam     Gahuk     Masil     Ukudz 
#> 0.7526608 0.7642241 1.0419771 1.0226038 0.9026693 0.9054574 1.2235610 1.1414595 
#>     Notoh     Kohik     Geham     Asaro     Uheto     Seuve     Nagad      Gama 
#> 0.8740846 0.9033828 0.8653172 0.9340946 0.9158561 0.8746127 0.7146865 0.7145186 
```
