# Q-analysis

Q-structure of a simplicial complex.

## Usage

``` r
q_analysis(A, simplicial_complex = FALSE, dimensions = FALSE)
```

## Arguments

- A:

  An incidence matrix

- simplicial_complex:

  Whether the incidence matrix is a simplices or simplicial complexes
  representation

- dimensions:

  Return the successively chains from high to low dimensions (\$q\$) and
  the number of components (\$Q_p\$)

## Value

This function return a q-analysis of a simplicial complex matrix

## References

Atkin, R. H. (1974). Mathematical structure in human affairs. New York:
Crane, Rusak.

Freeman, L. C. (1980). Q-analysis and the structure of friendship
networks. International Journal of Man-Machine Studies, 12(4), 367–378.
[doi:10.1016/S0020-7373(80)80021-6](https://doi.org/10.1016/S0020-7373%2880%2980021-6)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0,
  0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
  0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0
), byrow = TRUE, ncol = 19)
colnames(A) <- letters[1:ncol(A)]
rownames(A) <- 1:nrow(A)

q_analysis(A, simplicial_complex = TRUE)
#> $`2`
#>   component node
#> 1         1    3
#> 2         2   13
#> 
#> $`6`
#>   component node
#> 1         1    3
#> 2         2   13
#> 3         3    2
#> 4         4   19
#> 5         5   20
#> 6         6   21
#> 
#> $`9`
#>    component node
#> 1          1    3
#> 10         2   15
#> 11         3   25
#> 5          4   20
#> 7          5    4
#> 8          6    9
#> 9          7   11
#> 2          8   13
#> 4          8   19
#> 6          8   21
#> 3          9    2
#> 12         9   28
#> 
#> $`3`
#>    component node
#> 1          1    3
#> 2          1   13
#> 3          1    2
#> 4          1   19
#> 5          1   20
#> 6          1   21
#> 7          1    4
#> 8          1    9
#> 9          1   11
#> 10         1   15
#> 11         1   25
#> 12         1   28
#> 14         1    6
#> 15         1    8
#> 16         1   10
#> 18         1   14
#> 19         1   16
#> 20         1   17
#> 23         1   24
#> 24         1   26
#> 25         1   29
#> 13         2    5
#> 21         2   18
#> 17         3   12
#> 22         3   23
#> 
```
