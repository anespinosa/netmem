# Reciprocity of Katz and Powell

Reciprocity of Katz and Powell

## Usage

``` r
kp_reciprocity(G, fixed = FALSE, d = NULL, dichotomic = TRUE)
```

## Arguments

- G:

  A symmetric matrix object.

- fixed:

  Whether the choices are fixed or not

- d:

  Numeric value of the number of fixed choices.

- dichotomic:

  Whether the matrix is weighted or binary

## Value

This function gives a measurement of the tendency toward reciprocation
of choices.

## References

Katz, L. and Powell, J.H. (1955). "Measurement of the tendency toward
reciprocation of choice." Sociometry, 18:659-665.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
data(krackhardt_friends)
kp_reciprocity(krackhardt_friends, fixed = TRUE, d = 5)
#> [1] 0.2507937
```
