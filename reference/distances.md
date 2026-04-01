# Path distances

Distances between nodes using breadth-first search (BFS) or Dijkstra's
algorithm to find shortest path distances.

## Usage

``` r
bfs_ugraph(A, from = NULL)

count_geodesics(A)

short_path(A, from = NULL, to = NULL)

wlocal_distances(A, select = c("all", "in", "out"), from, to, path = c())

wall_distances(A, select = c("all", "in", "out"))
```

## Arguments

- A:

  A symmetric matrix object

- from:

  Node in which the path start

- to:

  Node in which the path end

- select:

  Whether to consider all sender and receiver ties of ego (`all`), only
  incoming ties (`in`), or outgoing ties (`out`). By default, `all`.

- path:

  Path of the nodes

## Value

This function returns the distances o shortest path distance between two
nodes for unweighted graph (`bfs_ugraph`, `count_geodesics` and
`short_path` respectively) and weighted graphs (`wlocal_distances` or
`wall_distances`)

## References

Dijkstra, E. W. (1959). A note on two problems in connexion with graphs.
Numerische Mathematik. 1: 269–271.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
# \donttest{
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  0, 0, 0, 1, 1, 0,
  0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0
), byrow = TRUE, nrow = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]

bfs_ugraph(A, from = "a")
#> $pointers
#> [1] NA  1  1  2  2  5
#> 
#> $distances
#> [1] 0 1 1 2 2 3
#> 
# }
# \donttest{
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  0, 0, 0, 1, 1, 0,
  0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0
), byrow = TRUE, nrow = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]

count_geodesics(A)
#> $counts
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    1    1    1    1    1    1
#> [2,]    0    1    0    1    1    1
#> [3,]    0    0    1    0    1    1
#> [4,]    0    0    0    1    0    0
#> [5,]    0    0    0    0    1    1
#> [6,]    0    0    0    0    0    1
#> 
#> $distances
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    0    1    1    2    2    3
#> [2,]  Inf    0  Inf    1    1    2
#> [3,]  Inf  Inf    0  Inf    1    2
#> [4,]  Inf  Inf  Inf    0  Inf  Inf
#> [5,]  Inf  Inf  Inf  Inf    0    1
#> [6,]  Inf  Inf  Inf  Inf  Inf    0
#> 
# }
# \donttest{
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  0, 0, 0, 1, 1, 0,
  0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 0, 0
), byrow = TRUE, nrow = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]

short_path(A, from = "a", to = "d")
#> [1] "a" "b" "d"
# }
# \donttest{
A <- matrix(
  c(
    0, 3, 3, 10, 15, 0, 0, 0,
    1, 0, 5, 2, 7, 0, 0, 0,
    3, 5, 0, 0, 0, 0, 0, 0,
    10, 2, 0, 0, 2, 7, 12, 0,
    11, 3, 0, 3, 0, 11, 2, 0,
    0, 0, 0, 7, 11, 0, 3, 2,
    0, 0, 0, 12, 2, 3, 0, 2,
    0, 0, 0, 0, 0, 2, 2, 0
  ),
  byrow = TRUE, ncol = 8, nrow = 8
)
rownames(A) <- c("a", "b", "s", "c", "d", "e", "f", "z")
colnames(A) <- rownames(A)
wlocal_distances(A, from = "a", to = "d")
#> $path
#> [1] "a" "b" "c" "d"
#> 
# }
# \donttest{
A <- matrix(
  c(
    0, 3, 3, 10, 15, 0, 0, 0,
    1, 0, 5, 2, 7, 0, 0, 0,
    3, 5, 0, 0, 0, 0, 0, 0,
    10, 2, 0, 0, 2, 7, 12, 0,
    11, 3, 0, 3, 0, 11, 2, 0,
    0, 0, 0, 7, 11, 0, 3, 2,
    0, 0, 0, 12, 2, 3, 0, 2,
    0, 0, 0, 0, 0, 2, 2, 0
  ),
  byrow = TRUE, ncol = 8, nrow = 8
)
rownames(A) <- c("a", "b", "s", "c", "d", "e", "f", "z")
colnames(A) <- rownames(A)
wall_distances(A, select = "in")
#> $fromTo
#> $fromTo$a
#> [1] "a" "b" "c" "d" "f" "z"
#> 
#> $fromTo$b
#> [1] "b" "c" "d" "f" "z"
#> 
#> $fromTo$s
#> [1] "s" "b" "c" "d" "f" "z"
#> 
#> $fromTo$c
#> [1] "c" "d" "f" "z"
#> 
#> $fromTo$d
#> [1] "d" "f" "z"
#> 
#> $fromTo$e
#> [1] "e" "z"
#> 
#> $fromTo$f
#> [1] "f" "z"
#> 
#> $fromTo$z
#> [1] "z"
#> 
#> 
# }
```
