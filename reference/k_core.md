# Generalized k-core

Generalized k-core for undirected, directed, weighted and multilevel
networks

## Usage

``` r
k_core(
  A,
  B1 = NULL,
  multilevel = FALSE,
  type = "in",
  digraph = FALSE,
  loops = FALSE,
  weighted = FALSE,
  alpha = 1
)
```

## Arguments

- A:

  A matrix object.

- B1:

  An incidence matrix for multilevel networks.

- multilevel:

  Whether the measure of k-core is for multilevel networks.

- type:

  Character string, “out” (outdegree), “in” (indegree) and “all”
  (degree)

- digraph:

  Whether the matrix is directed or undirected

- loops:

  Whether the diagonal of the matrix is considered or not

- weighted:

  Whether the measure of k-core is for valued matrices

- alpha:

  Sets the alpha parameter in the generalised measures from Opsahl et
  al. (2010)

## Value

This function return the k-core.

## References

Batagelj, V., & Zaveršnik, M. (2011). Fast algorithms for determining
(generalized) core groups in social networks. Advances in Data Analysis
and Classification, 5(2), 129–145.
[doi:10.1007/s11634-010-0079-y](https://doi.org/10.1007/s11634-010-0079-y)

Eidsaa, M., & Almaas, E. (2013). s-core network decomposition: A
generalization of \$k\$-core analysis to weighted networks. Physical
Review E, 88(6), 062819.
[doi:10.1103/PhysRevE.88.062819](https://doi.org/10.1103/PhysRevE.88.062819)

Seidman S (1983). 'Network structure and minimum degree'. Social
Networks, 5, 269-287.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A1 <- matrix(c(
  0, 1, 0, 0, 0,
  1, 0, 0, 1, 0,
  0, 0, 0, 1, 0,
  0, 1, 1, 0, 1,
  0, 0, 0, 1, 0
), byrow = TRUE, ncol = 5)
B1 <- matrix(c(
  1, 0, 0,
  1, 1, 0,
  0, 1, 0,
  0, 1, 0,
  0, 1, 1
), byrow = TRUE, ncol = 3)

k_core(A1, B1, multilevel = TRUE)
#> [1] 1 2 1 2 2
```
