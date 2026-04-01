# Hypergraphs

Hypergraph consist of a set of objects and a collection of subsets of
objects, in which each object belongs to at least one subset, and no
subset is empty (Berge, 1989)

## Usage

``` r
hypergraph(A, dual = TRUE, both = TRUE)
```

## Arguments

- A:

  An incidence matrix.

- dual:

  Whether to return the dual hypergraph (which rever the role of the
  pointes and the edges)

- both:

  Whether to return the hypergraph and the dual hypergraph

## Value

This function returns an adjacent list of the subsets of entities in the
hypergraph.

## References

Berge, C. (1973). Graphs and hypergraphs.Amsterdam: North-Holland.

Berge, C. (1989). Hypergraphs: Combinatorics of finite sets. Amsterdam:
North-Holland.

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  1, 0, 1,
  0, 1, 0,
  0, 1, 1,
  0, 0, 1,
  1, 1, 1,
  1, 1, 0
), byrow = TRUE, ncol = 3)
colnames(A) <- letters[1:ncol(A)]
rownames(A) <- letters[(ncol(A) + 1):(nrow(A) + ncol(A))]
hypergraph(A, both = TRUE)
#> $hypergraph
#> $hypergraph$d
#> [1] "a" "c"
#> 
#> $hypergraph$e
#> [1] "b"
#> 
#> $hypergraph$f
#> [1] "b" "c"
#> 
#> $hypergraph$g
#> [1] "c"
#> 
#> $hypergraph$h
#> [1] "a" "b" "c"
#> 
#> $hypergraph$i
#> [1] "a" "b"
#> 
#> 
#> $dual_hypergraph
#> $dual_hypergraph$a
#> [1] "d" "h" "i"
#> 
#> $dual_hypergraph$b
#> [1] "e" "f" "h" "i"
#> 
#> $dual_hypergraph$c
#> [1] "d" "f" "g" "h"
#> 
#> 
```
