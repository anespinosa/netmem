# Communities with agglomeration, label propagation or edge betweenness

Three classic ways of finding communities.

## Usage

``` r
community_greedy(A, weighted = FALSE)

community_label(A, weighted = FALSE, max_iter = 100)

community_betweenness(A, weighted = FALSE)
```

## Arguments

- A:

  A symmetric matrix object

- weighted:

  Whether the matrix is weighted

- max_iter:

  Maximum number of rounds of label propagation

## Value

These functions return the group of each node, the number of groups and
the modularity of the partition.

## Details

`community_greedy` starts with every node alone and merges at each step
the two groups that increase the modularity the most, keeping the
partition with the highest modularity (Clauset, Newman and Moore, 2004).

`community_label` gives every node a different label, and then each node
takes the label that most of its neighbours have, until no label
changes. It is fast but the result depends on the order in which the
nodes are visited, so it varies between runs (Raghavan, Albert and
Kumara, 2007).

`community_betweenness` removes, one at a time, the tie with the highest
edge betweenness, i.e. the tie through which most geodesics pass, as
those ties connect groups rather than being inside them. The components
that remain at each step give a partition, and the one with the highest
modularity is returned (Girvan and Newman, 2002).

## References

Clauset, A., Newman, M. E. J. and Moore, C. (2004). Finding community
structure in very large networks. Physical Review E, 70(6), 066111.
[doi:10.1103/PhysRevE.70.066111](https://doi.org/10.1103/PhysRevE.70.066111)

Girvan, M. and Newman, M. E. J. (2002). Community structure in social
and biological networks. Proceedings of the National Academy of
Sciences, 99(12), 7821–7826.
[doi:10.1073/pnas.122653799](https://doi.org/10.1073/pnas.122653799)

Raghavan, U. N., Albert, R. and Kumara, S. (2007). Near linear time
algorithm to detect community structures in large-scale networks.
Physical Review E, 76(3), 036106.
[doi:10.1103/PhysRevE.76.036106](https://doi.org/10.1103/PhysRevE.76.036106)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 0, 0, 0,
  1, 1, 0, 1, 0, 0,
  0, 0, 1, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  0, 0, 0, 1, 1, 0
), byrow = TRUE, ncol = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)

community_greedy(A)
#> $partition
#> a b c d e f 
#> 1 1 1 2 2 2 
#> 
#> $groups
#> [1] 2
#> 
#> $modularity
#> [1] 0.3571429
#> 
community_betweenness(A)
#> $partition
#> a b c d e f 
#> 1 1 1 2 2 2 
#> 
#> $groups
#> [1] 2
#> 
#> $modularity
#> [1] 0.3571429
#> 
set.seed(18051889)
community_label(A)
#> $partition
#> a b c d e f 
#> 1 1 1 2 2 2 
#> 
#> $groups
#> [1] 2
#> 
#> $modularity
#> [1] 0.3571429
#> 
```
