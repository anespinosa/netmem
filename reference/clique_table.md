# Clique table

Exploration of a 3-cliques, as the maximum number of three or more
actors who have all possible ties present among themselves

## Usage

``` r
clique_table(A, list_cliques = FALSE, number = FALSE)
```

## Arguments

- A:

  A symmetric matrix object.

- list_cliques:

  Whether to return the list of cliques.

- number:

  Number of triangles

## Value

This function return an edge list of actors participating in 3-cliques.

If `list_cliques = TRUE` it also return the list of cliques per nodes.
If `number = TRUE` the output returns the number of 3-cliques in the
matrix.

## References

Luce, R.D. and Perry, A.D. (1949). A method of matrix analysis of group
structure. Psychometrika, 14: 95-116.

Roethlisberger, F.J. and Dickson, W.J. (1939). Management and the
Worker. Harvard University Press, Cambridge, MA.

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0, 0, 1, 0,
  1, 0, 1, 0, 0, 0, 0, 0, 0,
  1, 1, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 1, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 1, 1, 0,
  0, 0, 0, 0, 0, 1, 0, 1, 0,
  1, 0, 0, 0, 0, 1, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0
), byrow = TRUE, ncol = 9)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)
clique_table(A, list_cliques = TRUE, number = TRUE)
#> $table
#>      node triad300
#> [1,] "a"  "1"     
#> [2,] "b"  "1"     
#> [3,] "c"  "1"     
#> [4,] "f"  "2"     
#> [5,] "g"  "2"     
#> [6,] "h"  "2"     
#> 
#> $n_triangles
#> [1] 2
#> 
#> $neighbours
#> $neighbours$a
#> [1] "a" "b" "c"
#> 
#> $neighbours$b
#> [1] "a" "b" "c"
#> 
#> $neighbours$c
#> [1] "a" "b" "c"
#> 
#> $neighbours$f
#> [1] "f" "g" "h"
#> 
#> $neighbours$g
#> [1] "f" "g" "h"
#> 
#> $neighbours$h
#> [1] "f" "g" "h"
#> 
#> 
```
