# Forbidden triad table

This function explores dyads and triads (Simmel, 1950), building from
the 'forbidden triad' (Granovetter, 1973). First, the minimum structure
is an isolated node, then dyads. Afterwards, different combinations of
'forbidden triads' are explored.

## Usage

``` r
dyad_triad_table(A, adjacency_list = FALSE, min = NULL, max = NULL)
```

## Arguments

- A:

  A symmetric matrix object.

- adjacency_list:

  Whether to return the adjacency list of triads 201 per node.

- min:

  Numeric constant, lower limit on the size of the triads 201 to find.
  NULL means no limit, ie. it is the same as 0.

- max:

  Numeric constant, upper limit on the size of the triads 201 to find.
  NULL means no limit.

## Value

This function return the list of triads that each node belong.

If `adjacency_list = TRUE` it also return the adjacency list of the
'forbidden triads' per node.

## References

Granovetter, M.S. (1973). The Strength of Weak Ties. American Journal of
Sociology. 78 (6): 1360–80.
[doi:10.1086/225469](https://doi.org/10.1086/225469) .

Simmel, G. (1950). Individual and Society. In K. H. Wolff (Ed.), The
Sociology of George Simmel. New York: Free Press.

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 1, 0,
  1, 0, 1, 0, 0,
  1, 1, 0, 0, 0,
  1, 0, 0, 0, 1,
  0, 0, 0, 1, 0
), byrow = TRUE, ncol = 5)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]

dyad_triad_table(A, adjacency_list = TRUE, min = 3)
#> $nodes
#>   node Triad201
#> 1    a        1
#> 2    b        1
#> 3    c        1
#> 
#> $adjacency_list
#> $adjacency_list$a
#> [1] "abc" "abd" "acd"
#> 
#> $adjacency_list$b
#> [1] "abc"
#> 
#> $adjacency_list$c
#> [1] "abc"
#> 
#> 
```
