# Small world network

Random network of Watts and Strogatz (1998), which has the short
distances of a random network and the high transitivity of a regular
one.

## Usage

``` r
small_world(n, neighbours = 2, p = 0.05)
```

## Arguments

- n:

  The number of nodes

- neighbours:

  The number of closest nodes on each side that every node is tied to in
  the ring

- p:

  Probability of rewiring each tie

## Value

This function returns a symmetric matrix.

## Details

The network starts as a ring in which every node is tied to its
`neighbours` closest nodes on each side. Then each tie is rewired with
probability `p`: one of its ends is replaced by a node chosen at random,
avoiding loops and repeated ties. With `p = 0` the ring is left as it
is, and with `p = 1` every tie is rewired.

## References

Watts, D. J. and Strogatz, S. H. (1998). Collective dynamics of
'small-world' networks. Nature, 393(6684), 440–442.
[doi:10.1038/30918](https://doi.org/10.1038/30918)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
set.seed(18051889)
A <- small_world(20, neighbours = 2, p = 0.05)
gen_degree(A, digraph = FALSE)
#>  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
#>  5  5  4  4  3  4  4  4  4  4  3  3  4  4  4  4  4  4  4  5 
```
