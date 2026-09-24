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

  Whether to return the adjacency list of the triads per node.

- min:

  Numeric constant, lower limit on the number of forbidden triads (201)
  of which a node is the centre. NULL means no limit.

- max:

  Numeric constant, upper limit on the number of forbidden triads (201)
  of which a node is the centre. NULL means no limit.

## Value

This function returns a data frame with the triads of each node: the
`node`, the number of the `triad`, its `members` and its `type`.

If `adjacency_list = TRUE` it also return the adjacency list of the
triads per node.

## Details

For each node, every pair of its neighbours forms a triad with the node
at its centre. The triad is a forbidden triad (type `201`) when the two
neighbours are not tied, which Granovetter (1973) argued is unlikely
when both ties are strong, and it is closed (type `300`) when they are.
A node with a single neighbour is listed with its dyad (type `102`) and
an isolated node alone (type `003`). The underlying graph of the network
is used.

The same triad receives the same number in `triad` for every node that
lists it: a closed triad is listed by its three nodes, and a forbidden
triad only by its centre.

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

dyad_triad_table(A)
#>   node triad members type
#> 1    a     1   a|b|c  300
#> 2    a     2   a|b|d  201
#> 3    a     3   a|c|d  201
#> 4    b     1   a|b|c  300
#> 5    c     1   a|b|c  300
#> 6    d     4   a|d|e  201
#> 7    e     5     d|e  102

# Nodes at the centre of at least two forbidden triads
dyad_triad_table(A, adjacency_list = TRUE, min = 2)
#> $nodes
#>   node triad members type
#> 1    a     1   a|b|c  300
#> 2    a     2   a|b|d  201
#> 3    a     3   a|c|d  201
#> 
#> $adjacency_list
#> $adjacency_list$a
#> [1] "a|b|c" "a|b|d" "a|c|d"
#> 
#> 
```
