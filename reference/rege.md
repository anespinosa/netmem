# Regular equivalence

REGE algorithm (White and Reitz, 1983; Borgatti and Everett, 1993): two
nodes are regularly equivalent when they are connected to nodes that are
themselves equivalent, even if they are not connected to the same nodes.

## Usage

``` r
rege(A, iter = 3)
```

## Arguments

- A:

  A square matrix, which can be valued

- iter:

  Number of iterations

## Value

This function returns a matrix with the regular equivalence of every
pair of nodes, between zero and one.

## Details

Structural equivalence asks for the same neighbours, while regular
equivalence only asks for neighbours that play the same role. Each alter
of a node is matched with the alter of the other node that resembles it
the most, and the similarities are computed again with the matches of
the previous iteration.

## References

Borgatti, S. P. and Everett, M. G. (1993). Two algorithms for computing
regular equivalence. Social Networks, 15(4), 361–376.
[doi:10.1016/0378-8733(93)90012-A](https://doi.org/10.1016/0378-8733%2893%2990012-A)

White, D. R. and Reitz, K. P. (1983). Graph and semigroup homomorphisms
on networks of relations. Social Networks, 5(2), 193–234.
[doi:10.1016/0378-8733(83)90025-4](https://doi.org/10.1016/0378-8733%2883%2990025-4)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
# Two managers with different subordinates play the same role
A <- matrix(c(
  0, 1, 1, 0, 0, 0, 0,
  0, 0, 0, 1, 1, 0, 0,
  0, 0, 0, 0, 0, 1, 1,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0
), byrow = TRUE, ncol = 7)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

round(rege(A), 3)
#>      a    b    c   d   e   f   g
#> a 1.00 0.32 0.32 0.0 0.0 0.0 0.0
#> b 0.32 1.00 1.00 0.2 0.2 0.2 0.2
#> c 0.32 1.00 1.00 0.2 0.2 0.2 0.2
#> d 0.00 0.20 0.20 1.0 1.0 1.0 1.0
#> e 0.00 0.20 0.20 1.0 1.0 1.0 1.0
#> f 0.00 0.20 0.20 1.0 1.0 1.0 1.0
#> g 0.00 0.20 0.20 1.0 1.0 1.0 1.0
```
