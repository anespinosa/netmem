# Simplicial complexes

incidence matrix of simplexes or cliques

## Usage

``` r
simplicial_complexes(A, zero_simplex = FALSE, projection = FALSE)
```

## Arguments

- A:

  A symmetric matrix object.

- zero_simplex:

  Whether to include the zero simple.

- projection:

  Whether to return the links between actors (i.e., rows) through their
  shared linking events (i.e., columns).

## Value

This function return an incidence matrix of actors participating in
simplices or simplicial complexes

## References

Atkin, R. H. (1974). Mathematical structure in human affairs. New York:
Crane, Rusak.

Freeman, L. C. (1980). Q-analysis and the structure of friendship
networks. International Journal of Man-Machine Studies, 12(4), 367–378.
[doi:10.1016/S0020-7373(80)80021-6](https://doi.org/10.1016/S0020-7373%2880%2980021-6)

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0, 0, 1, 0,
  1, 0, 1, 0, 0, 0, 0, 0, 0,
  1, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 1, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 1, 1, 0,
  0, 0, 0, 0, 0, 1, 0, 1, 0,
  1, 0, 0, 0, 0, 1, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0
), byrow = TRUE, ncol = 9)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)
simplicial_complexes(A, zero_simplex = FALSE)
#>   1 2 3 4 5 6 7 8 9 10 11
#> a 1 0 0 0 1 1 1 0 0  0  0
#> b 1 0 0 0 1 0 0 1 0  0  0
#> c 1 0 0 0 0 1 0 1 0  0  0
#> d 0 0 0 0 0 0 0 0 1  1  0
#> e 0 0 0 0 0 0 0 0 1  0  0
#> f 0 1 0 1 0 0 0 0 0  1  1
#> g 0 0 1 1 0 0 0 0 0  0  1
#> h 0 1 1 1 0 0 1 0 0  0  0
```
