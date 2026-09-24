# Modularity

Modularity of a partition of the nodes (Newman and Girvan, 2004): the
proportion of the ties that are within groups, minus the proportion that
would be expected if the ties were distributed at random keeping the
degree of every node.

## Usage

``` r
modularity_score(
  A,
  partition,
  method = c("newman", "linkrank"),
  digraph = FALSE,
  weighted = FALSE,
  resolution = 1,
  damping = 0.85
)
```

## Arguments

- A:

  A square matrix

- partition:

  A vector with the group of each node

- method:

  Whether to use the modularity of `newman` (default) or of `linkrank`

- digraph:

  Whether the matrix is directed or undirected

- weighted:

  Whether the matrix is weighted

- resolution:

  Weight given to the expected ties. Values above one give smaller
  groups

- damping:

  Probability of following a tie in the random walk of the `linkrank`
  method

## Value

This function returns the modularity of the partition.

## Details

The modularity is positive when the nodes of a group are connected among
themselves more often than expected. It is used to compare partitions of
the same network, as its maximum depends on the network. For directed
networks, the expected ties use the out-degree of the sender and the
in-degree of the receiver (Arenas et al., 2007).

The `linkrank` method (Kim, Son and Jeong, 2010) replaces the ties by
the flow of a random walker: the value of a tie is the PageRank of the
sender times the probability that the walker uses that tie, and the
expected value is the product of the PageRank of both nodes. It takes
the direction of the ties into account, which the modularity of Arenas
et al. does only through the degrees.

Modularity has a resolution limit (Fortunato and Barthelemy, 2007): it
does not detect groups below a size that depends on the size of the
network. The `resolution` parameter changes the weight of the expected
ties to look for smaller or larger groups.

## References

Arenas, A., Duch, J., Fernandez, A. and Gomez, S. (2007). Size reduction
of complex networks preserving modularity. New Journal of Physics, 9(6),
176.
[doi:10.1088/1367-2630/9/6/176](https://doi.org/10.1088/1367-2630/9/6/176)

Fortunato, S. and Barthelemy, M. (2007). Resolution limit in community
detection. Proceedings of the National Academy of Sciences, 104(1),
36–41.
[doi:10.1073/pnas.0605965104](https://doi.org/10.1073/pnas.0605965104)

Kim, Y., Son, S.-W. and Jeong, H. (2010). Finding communities in
directed networks. Physical Review E, 81(1), 016103.
[doi:10.1103/PhysRevE.81.016103](https://doi.org/10.1103/PhysRevE.81.016103)

Newman, M. E. J. and Girvan, M. (2004). Finding and evaluating community
structure in networks. Physical Review E, 69(2), 026113.
[doi:10.1103/PhysRevE.69.026113](https://doi.org/10.1103/PhysRevE.69.026113)

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

modularity_score(A, partition = c(1, 1, 1, 2, 2, 2))
#> [1] 0.3571429
modularity_score(A, partition = c(1, 1, 1, 2, 2, 2), method = "linkrank", digraph = TRUE)
#> [1] 0.3071168
```
