# Conditional uniform graph test

Compares a statistic of the observed network with its distribution in
random networks that share some of its features (Anderson, Butts and
Carley, 1999; Wasserman and Faust, 1994).

## Usage

``` r
cug_test(
  A,
  FUN,
  cmode = c("edges", "size", "dyad"),
  reps = 1000,
  digraph = TRUE,
  ...
)
```

## Arguments

- A:

  A square matrix

- FUN:

  A function that takes a matrix and returns a single number

- cmode:

  The feature that the random networks share with the observed one:
  `size`, `edges` (default) or `dyad`

- reps:

  Number of random networks

- digraph:

  Whether the matrix is directed or undirected

- ...:

  Other arguments passed to `FUN`

## Value

This function returns the observed statistic, the mean and the standard
deviation of the distribution, and the proportion of random networks
with a statistic greater or equal, and lower or equal, than the observed
one.

## Details

The random networks are drawn from a uniform distribution conditioned
on:

`size`: only the number of nodes, so every tie is present with
probability one half,

`edges`: the number of nodes and the number of ties,

`dyad`: the dyad census, i.e. the number of mutual, asymmetric and null
dyads (U\|MAN).

The test says whether the statistic is higher or lower than expected
once those features are taken into account. Conditioning on the dyad
census, for instance, removes the tendency towards reciprocity before
looking at the triads.

## References

Anderson, B. S., Butts, C. and Carley, K. (1999). The interaction of
size and density with graph-level indices. Social Networks, 21(3),
239–267.
[doi:10.1016/S0378-8733(99)00011-8](https://doi.org/10.1016/S0378-8733%2899%2900011-8)

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 0, 0, 0,
  1, 1, 0, 1, 0, 0,
  0, 0, 1, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  0, 0, 0, 1, 1, 0
), byrow = TRUE, ncol = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

set.seed(18051889)
cug_test(A,
  FUN = function(x) trans_coef(x, method = "global"),
  cmode = "edges", reps = 100, digraph = FALSE
)
#> $observed
#> [1] 0.6
#> 
#> $mean
#> [1] 0.3826693
#> 
#> $sd
#> [1] 0.1999829
#> 
#> $p_greater
#> [1] 0.14
#> 
#> $p_lower
#> [1] 0.87
#> 
#> $distribution
#>   [1] 0.2500000 0.2727273 0.6428571 0.3000000 0.4615385 0.5000000 0.5000000
#>   [8] 0.4615385 0.4615385 0.5000000 0.4615385 0.2727273 0.2727273 0.2727273
#>  [15] 0.0000000 0.4000000 0.4615385 0.2727273 0.0000000 0.4615385 0.3000000
#>  [22] 0.5000000 0.5000000 0.5000000 0.2727273 0.2500000 0.2500000 0.4615385
#>  [29] 0.4285714 0.8000000 0.2727273 0.0000000 0.6428571 0.2727273 0.0000000
#>  [36] 0.2727273 0.2500000 0.0000000 0.6428571 0.4615385 0.6428571 0.4615385
#>  [43] 0.4615385 0.3000000 0.8000000 0.5454545 0.8000000 0.2727273 1.0000000
#>  [50] 0.4615385 0.5000000 0.2500000 0.2727273 0.6000000 0.4615385 0.4615385
#>  [57] 0.2727273 0.6428571 0.5000000 0.2727273 0.4285714 0.5000000 0.5000000
#>  [64] 0.5454545 0.4000000 0.4615385 0.4000000 0.2727273 0.5000000 0.5000000
#>  [71] 0.8000000 0.2727273 0.2500000 0.4285714 0.2727273 0.2727273 0.0000000
#>  [78] 0.0000000 0.2500000 0.3000000 0.2727273 0.6428571 0.0000000 0.2727273
#>  [85] 0.2500000 0.5000000 0.2727273 0.4285714 0.4615385 0.6428571 0.2727273
#>  [92] 0.0000000 0.2727273 0.2500000 0.2500000 0.0000000 0.6428571 0.4615385
#>  [99] 0.2727273 0.4615385
#> 
#> $cmode
#> [1] "edges"
#> 
```
