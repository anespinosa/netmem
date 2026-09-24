# Constraint

Everett and Borgatti specification of the constraint measure for binary,
directed and valued matrices

## Usage

``` r
eb_constraint(A, ego = NULL, digraph = FALSE, weighted = FALSE)
```

## Arguments

- A:

  A matrix object

- ego:

  Name of ego in the matrix

- digraph:

  Whether the matrix is directed or undirected

- weighted:

  Whether the matrix is weighted or not

## Value

This function returns term 1, 2 and 3, the normalization and the maximum
value of the specification of Everett and Borgatti (2020), and the
constraint of Burt (1992).

## Details

The constraint of Burt (1992) is computed in the ego network, from the
proportion \\p\_{ij}\\ of the ties of each node \\i\\ that go to \\j\\,
and is split in the three terms of Everett and Borgatti (2020: Eq. 2):
\$\$\sum_j p\_{ij}^2 + 2 \sum_j p\_{ij} \sum_q p\_{iq} p\_{qj} + \sum_j
\left(\sum_q p\_{iq} p\_{qj}\right)^2\$\$ For binary undirected
networks, the first term is one over the number of alters \\N\\.

Burt (1992) uses the ties in both directions, \\p\_{ij} \propto
a\_{ij} + a\_{ji}\\, so the constraint of a directed network is that of
the undirected valued network \\A + A^T\\, in which a reciprocated tie
counts twice and an unreciprocated tie once (Everett and Borgatti, 2020:
53).

The normalization is \\(c - 1/N) / (c\_{max} - 1/N)\\, where \\1/N\\ is
the minimum and \\c\_{max}\\ the maximum constraint of an ego with \\N\\
alters. The maximum is reached in a complete ego network or in a shadow
ego network, in which one alter is tied to all the others and there are
no other ties among alters (Everett and Borgatti, 2020: Eq. 4, 6, 7, 8
and 9). For valued networks the maximum depends on the smallest (\\m\\)
and the largest (\\M\\) value of the ties in the ego network, and binary
networks are the case \\m = M = 1\\. The maximum is a conjecture of
Everett and Borgatti, checked by enumerating ego networks. An ego with a
single alter has a normalized constraint of one.

## References

Burt, R.S., 1992. Structural Holes: the Social Structure of Competition.
Harvard University Press, Cambridge.

Everett, M.G. and Borgatti, S., 2020. Unpacking Burt's constraint
measure. Social Networks 62, pp. 50-57.
[doi:10.1016/j.socnet.2020.02.001](https://doi.org/10.1016/j.socnet.2020.02.001)

## Author

Alejandro Espinosa-Rada

## Examples

``` r

A <- matrix(c(
  0, 1, 1, 0, 0, 1,
  1, 0, 1, 0, 0, 1,
  1, 1, 0, 0, 0, 1,
  0, 0, 0, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  1, 1, 1, 1, 1, 0
), ncol = 6, byrow = TRUE)

rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]
eb_constraint(A, ego = "f")
#> $results
#>   term1 term2 term3 constraint normalization
#> f   0.2  0.24 0.073      0.513         0.699
#> 
#> $maximum
#>     f 
#> 0.648 
#> 

# Directed network: f -> a is not reciprocated
D <- A
D["a", "f"] <- 0
eb_constraint(D, ego = "f", digraph = TRUE)
#> $results
#>   term1 term2 term3 constraint normalization
#> f  0.21 0.237 0.075      0.522         0.517
#> 
#> $maximum
#>     f 
#> 0.823 
#> 

# Valued network
W <- A
W["f", "a"] <- W["a", "f"] <- 3
eb_constraint(W, ego = "f", weighted = TRUE)
#> $results
#>   term1 term2 term3 constraint normalization
#> f 0.265 0.199 0.055      0.519         0.429
#> 
#> $maximum
#>     f 
#> 0.944 
#> 
```
