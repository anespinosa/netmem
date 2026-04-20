# Zone-2 sampling from second-mode

Second-zone multilevel sampling considering a second-mode focal actor

## Usage

``` r
zone_sample(A, X, ego = TRUE, core = FALSE)
```

## Arguments

- A:

  A symmetric matrix object.

- X:

  X an incidence matrix object.

- ego:

  Whether to add or not ego into the subgraph.

- core:

  Whether to add actors at distance one from ego

## Value

This function return a list of second-zone subgraphs using as a focal
actor the second-mode of the multilevel network.

## References

Espinosa-Rada, A. (2021). A Network Approach for the Sociological Study
of Science: Modelling Dynamic Multilevel Networks.
\[PhD\](https://research.manchester.ac.uk/en/studentTheses/a-network-approach-for-the-sociological-study-of-science-and-know).
The University of Manchester.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0,
  0, 1, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0
), byrow = TRUE, ncol = 8)
colnames(A) <- c("1", "2", "3", "4", "5", "6", "7", "8")
rownames(A) <- c("1", "2", "3", "4", "5", "6", "7", "8")

X <- matrix(c(
  1, 0, 0, 0,
  1, 0, 0, 0,
  1, 0, 1, 0,
  0, 1, 1, 0,
  0, 1, 1, 1,
  0, 1, 0, 0,
  0, 0, 0, 0,
  0, 0, 0, 1
), byrow = TRUE, ncol = 4)
colnames(X) <- c("a", "b", "c", "d")
rownames(X) <- c("1", "2", "3", "4", "5", "6", "7", "8")

set.seed(18051889)
zone_sample(A, X, core = TRUE)
#> $a
#> IGRAPH 6f33901 DN-- 7 16 -- 
#> + attr: name (v/c), core (v/n)
#> + edges from 6f33901 (vertex names):
#>  [1] 1->2 1->a 2->3 2->a 3->2 3->4 3->a 3->c 4->b 4->c a->1 a->2 a->3 b->4 c->3
#> [16] c->4
#> 
#> $b
#> IGRAPH 370d863 DN-- 8 18 -- 
#> + attr: name (v/c), core (v/n)
#> + edges from 370d863 (vertex names):
#>  [1] 3->4 3->a 3->c 4->b 4->c 5->b 5->c 5->d 6->4 6->b a->3 b->4 b->5 b->6 c->3
#> [16] c->4 c->5 d->5
#> 
#> $c
#> IGRAPH c240a5b DN-- 9 22 -- 
#> + attr: name (v/c), core (v/n)
#> + edges from c240a5b (vertex names):
#>  [1] 2->3 2->a 3->2 3->4 3->a 3->c 4->b 4->c 5->b 5->c 5->d 6->4 6->b a->2 a->3
#> [16] b->4 b->5 b->6 c->3 c->4 c->5 d->5
#> 
#> $d
#> IGRAPH 3795571 DN-- 5 8 -- 
#> + attr: name (v/c), core (v/n)
#> + edges from 3795571 (vertex names):
#> [1] 5->b 5->c 5->d 8->d b->5 c->5 d->5 d->8
#> 
```
