# Supra-adjacency matrix

Arranges the layers of a multiplex network, in which the same actors are
connected by several relations, into a single matrix of actor-layer
pairs (De Domenico et al., 2013; Kivela et al., 2014).

## Usage

``` r
supra_adjacency(
  layers,
  coupling = c("categorical", "ordinal", "none"),
  weight = 1,
  sparse = FALSE
)
```

## Arguments

- layers:

  A list of square matrices of the same order, one for each layer, with
  the same actors in the same order

- coupling:

  Whether the copies of an actor are joined in every pair of layers
  (`categorical`, default), only in consecutive layers (`ordinal`), or
  not at all (`none`)

- weight:

  The value of the ties between the copies of the same actor

- sparse:

  Whether to return a sparse matrix of the `Matrix` package

## Value

This function returns the supra-adjacency matrix of the layers.

## Details

The supra-adjacency matrix has one row and one column for each actor in
each layer. The blocks of the diagonal are the layers, and the blocks
outside it are the coupling between the layers, which joins the copies
of the same actor.

`coupling = "categorical"` (default) joins the copies of an actor in
every pair of layers, which is the usual choice when the layers are
different relations. `"ordinal"` joins them only in consecutive layers,
for layers that follow an order, such as time. `"none"` leaves the
layers apart, and the result is block diagonal.

The `weight` of the coupling is the value given to those ties, and it
sets how much the layers are held together in measures computed on the
whole structure.

The rows and the columns are named after the actor and the layer, as
`actor_layer`, taking the names of the matrices and of the list of
layers when they have them.

## References

De Domenico, M., Sole-Ribalta, A., Cozzo, E., Kivela, M., Moreno, Y.,
Porter, M. A., Gomez, S. and Arenas, A. (2013). Mathematical formulation
of multilayer networks. Physical Review X, 3(4), 041022.
[doi:10.1103/PhysRevX.3.041022](https://doi.org/10.1103/PhysRevX.3.041022)

Kivela, M., Arenas, A., Barthelemy, M., Gleeson, J. P., Moreno, Y. and
Porter, M. A. (2014). Multilayer networks. Journal of Complex Networks,
2(3), 203-271.
[doi:10.1093/comnet/cnu016](https://doi.org/10.1093/comnet/cnu016)

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
  0, 0, 1,
  0, 0, 0,
  1, 0, 0
), byrow = TRUE, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))

supra_adjacency(list(advice = A1, friendship = A2))
#>              a_advice b_advice c_advice a_friendship b_friendship c_friendship
#> a_advice            0        1        0            1            0            0
#> b_advice            1        0        1            0            1            0
#> c_advice            0        1        0            0            0            1
#> a_friendship        1        0        0            0            0            1
#> b_friendship        0        1        0            0            0            0
#> c_friendship        0        0        1            1            0            0
#> attr(,"actors")
#> [1] "a" "b" "c"
#> attr(,"layers")
#> [1] "advice"     "friendship"

# Layers that follow an order are coupled only with the next one
supra_adjacency(list(A1, A2), coupling = "ordinal")
#>      a_L1 b_L1 c_L1 a_L2 b_L2 c_L2
#> a_L1    0    1    0    1    0    0
#> b_L1    1    0    1    0    1    0
#> c_L1    0    1    0    0    0    1
#> a_L2    1    0    0    0    0    1
#> b_L2    0    1    0    0    0    0
#> c_L2    0    0    1    1    0    0
#> attr(,"actors")
#> [1] "a" "b" "c"
#> attr(,"layers")
#> [1] "L1" "L2"
```
