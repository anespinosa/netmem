# Fractional approach

Matrix transformation from incidence matrices to citation networks,
fractional counting for co-citation or fractional counting for
bibliographic coupling

## Usage

``` r
fractional_approach(
  A1,
  A2,
  approach = c("citation", "cocitation", "bcoupling")
)
```

## Arguments

- A1:

  From incidence matrix (e.g. paper and authors)

- A2:

  To incidence matrix (e.g. author to paper)

- approach:

  Character string, “citation”, “cocitation” and “bcoupling”

## Value

Return a type of "citation network"

## References

Batagelj, V. (2020). Analysis of the Southern women network using
fractional approach. Social Networks, 68, 229-236
[doi:10.1016/j.socnet.2021.08.001](https://doi.org/10.1016/j.socnet.2021.08.001)

Batagelj, V., & Cerinšek, M. (2013). On bibliographic networks.
Scientometrics, 96(3), 845–864.
[doi:10.1007/s11192-012-0940-1](https://doi.org/10.1007/s11192-012-0940-1)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A1 <- matrix(c(
  1, 0, 0, 0,
  0, 1, 0, 0,
  0, 1, 1, 1,
  0, 0, 0, 0,
  0, 0, 0, 1
), byrow = TRUE, ncol = 4)

A2 <- matrix(c(
  1, 1, 1, 0, 0,
  0, 0, 1, 0, 0,
  0, 0, 1, 1, 0,
  0, 0, 0, 1, 1
), byrow = TRUE, ncol = 5)

fractional_approach(A1, A2)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    1    1    0    0
#> [2,]    1    1    1    0    0
#> [3,]    1    1    6    4    2
#> [4,]    0    0    4    5    3
#> [5,]    0    0    2    3    2
```
