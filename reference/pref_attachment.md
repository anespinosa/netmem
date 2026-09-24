# Preferential attachment network

Random network of Barabasi and Albert (1999), in which the nodes that
already have more ties are more likely to receive the ties of the nodes
that arrive.

## Usage

``` r
pref_attachment(n, m = 1, power = 1, digraph = FALSE)
```

## Arguments

- n:

  The number of nodes

- m:

  The number of ties that every new node creates

- power:

  The power of the degree in the probability of being chosen

- digraph:

  Whether the ties of the new nodes are directed towards the nodes that
  are already there

## Value

This function returns a matrix.

## Details

The network starts with `m` nodes without ties. Every new node creates
`m` ties with the nodes that are already there, choosing each of them
with a probability proportional to their degree raised to `power`. With
`power = 0` the nodes are chosen at random, and the higher the power,
the more the ties concentrate in a few nodes.

## References

Barabasi, A. L. and Albert, R. (1999). Emergence of scaling in random
networks. Science, 286(5439), 509–512.
[doi:10.1126/science.286.5439.509](https://doi.org/10.1126/science.286.5439.509)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
set.seed(18051889)
A <- pref_attachment(20, m = 2)
gen_degree(A, digraph = FALSE)
#>  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
#>  5  1 12  6  7  3  5  2  4  2  4  4  2  2  3  2  2  2  2  2 
```
