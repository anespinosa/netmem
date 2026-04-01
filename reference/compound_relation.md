# Relational composition

This function returns the relational composition of the given matrices.
The compound relations define the paths and the social process flows of
the given matrices (Pattison, 1993). However, those whom they link may
or may not be aware of them. The compound relations allow us to identify
"the possibly very long and devious chains of effects propagating
withing concrete social systems through links of various kinds" (Lorrain
& White, 1971: 50).

## Usage

``` r
compound_relation(l = list(), comp = 3, matrices = FALSE, equate = FALSE)
```

## Arguments

- l:

  A list of matrices.

- comp:

  A number with the length of paths to form the compound relation.

- matrices:

  Whether to return the resulting matrices of the compound relations.

- equate:

  Whether to return the semigroup equations.

## Value

This function provides the composition or concatenation of compound
relations and the primitives of the matrices.

## References

Boorman, Scott A. and White, Harrison C. (1976) Social Structure from
Multiple Networks. II. Role Structures. American Journal of Sociology.
81(6): 1384-1446.

Lorrain, Francois and White, Harrison C. (1971) Structural Equivalence
of Individuals in Social Networks. Journal of Mathematical Sociology. 1:
49-80

Pattison, Philippa (1993) Algebraic Models for Social Networks.
Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 0, 0,
  1, 0, 0, 0,
  1, 1, 0, 1,
  0, 0, 1, 0
), byrow = TRUE, ncol = 4)
rownames(A) <- letters[1:NCOL(A)]
colnames(A) <- rownames(A)

B <- matrix(c(
  0, 1, 0, 0,
  1, 0, 0, 0,
  0, 0, 0, 1,
  0, 0, 1, 0
), byrow = TRUE, ncol = 4)
rownames(B) <- letters[1:NCOL(B)]
colnames(B) <- rownames(B)

cmp <- compound_relation(list(A, B), comp = 2, matrices = TRUE, equate = TRUE)
cmp$compound_relations
#> [1] "a"  "b"  "ab" "aa" "ba" "bb"
cmp$compound_matrices
#> $a
#>   a b c d
#> a 0 1 0 0
#> b 1 0 0 0
#> c 1 1 0 1
#> d 0 0 1 0
#> 
#> $b
#>   a b c d
#> a 0 1 0 0
#> b 1 0 0 0
#> c 0 0 0 1
#> d 0 0 1 0
#> 
#> $ab
#>   a b c d
#> a 1 0 0 0
#> b 0 1 0 0
#> c 1 1 1 0
#> d 0 0 0 1
#> 
#> $aa
#>   a b c d
#> a 1 0 0 0
#> b 0 1 0 0
#> c 1 1 1 0
#> d 1 1 0 1
#> 
#> $ba
#>   a b c d
#> a 1 0 0 0
#> b 0 1 0 0
#> c 0 0 1 0
#> d 1 1 0 1
#> 
#> $bb
#>   a b c d
#> a 1 0 0 0
#> b 0 1 0 0
#> c 0 0 1 0
#> d 0 0 0 1
#> 
cmp$equated
#> [1] "No reduced equation"
```
