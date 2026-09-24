# Aggregation of the layers

Reduces a multiplex network to a single matrix, in which the actors are
joined when they are connected in the layers (Battiston et al., 2014; De
Domenico et al., 2015).

## Usage

``` r
aggregate_layers(layers, method = c("sum", "binary", "mean"), l = NULL)
```

## Arguments

- layers:

  A list of square matrices of the same order, or a supra-adjacency
  matrix with `l` layers

- method:

  Whether the ties are added (`sum`, default), made binary (`binary`) or
  averaged (`mean`)

- l:

  The number of layers, when `layers` is a supra-adjacency matrix that
  does not come from
  [`supra_adjacency()`](https://anespinosa.github.io/netmem/reference/supra_adjacency.md)

## Value

This function returns a square matrix with the actors of the layers.

## Details

`method = "sum"` (default) adds the layers, so the value of a tie is the
number of layers in which it is present, which Battiston et al. (2014)
call the overlapping network. `"binary"` gives one to every tie that is
present in at least one layer, and `"mean"` divides the sum by the
number of layers.

The layers are given as a list of matrices, or as the supra-adjacency
matrix of
[`supra_adjacency()`](https://anespinosa.github.io/netmem/reference/supra_adjacency.md)
together with the number of layers `l`, in which case the coupling
between the layers is ignored.

Aggregating loses the information of which layer each tie belongs to,
and measures computed on the aggregated network can differ from the ones
computed on the layers (De Domenico et al., 2015).

## References

Battiston, F., Nicosia, V. and Latora, V. (2014). Structural measures
for multiplex networks. Physical Review E, 89(3), 032804.
[doi:10.1103/PhysRevE.89.032804](https://doi.org/10.1103/PhysRevE.89.032804)

De Domenico, M., Nicosia, V., Arenas, A. and Latora, V. (2015).
Structural reducibility of multilayer networks. Nature Communications,
6, 6864. [doi:10.1038/ncomms7864](https://doi.org/10.1038/ncomms7864)

## Author

Alejandro Espinosa-Rada

## Examples

``` r

A1 <- matrix(c(
  0, 1, 0,
  1, 0, 1,
  0, 1, 0
), byrow = TRUE, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))

A2 <- matrix(c(
  0, 1, 1,
  1, 0, 0,
  1, 0, 0
), byrow = TRUE, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))

# The tie a-b is in both layers
aggregate_layers(list(A1, A2))
#>   a b c
#> a 0 2 1
#> b 2 0 1
#> c 1 1 0
aggregate_layers(list(A1, A2), method = "binary")
#>   a b c
#> a 0 1 1
#> b 1 0 1
#> c 1 1 0

# The same from the supra-adjacency matrix, which knows its layers
aggregate_layers(supra_adjacency(list(A1, A2)))
#>   a b c
#> a 0 2 1
#> b 2 0 1
#> c 1 1 0
```
