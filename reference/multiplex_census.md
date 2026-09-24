# Multiplex triad census

This function counts the different subgraphs of three nodes in a
multiplex directed and undirected network.

## Usage

``` r
multiplex_census(A, B, merge = c("none", "overlap"))
```

## Arguments

- A:

  A directed matrix object.

- B:

  An undirected matrix object. A directed matrix is replaced by its
  underlying graph.

- merge:

  Whether to merge the classes that give the same overlapped triad
  (`overlap`) or not (`none`, default).

## Value

This function gives the number of triples in each class, named by the
type of the first network and the position of the edges of the second.

## Details

Each triple of nodes is classified by its type in the first (directed)
network, one of the 16 types of the triad census (Holland and Leinhardt,
1976), and by the position of the edges of the second (undirected)
network in that triad (Espinosa-Rada, 2021: Figure 12). Each type of the
first network is drawn in fixed positions, bottom left, top and bottom
right, and the edges of the second network are named by where they fall:
102a (bottom left to top), 102b (bottom left to bottom right) and 102c
(top to bottom right); the two-paths by their centre, 201a (bottom
left), 201b (bottom right) and 201c (top). Positions that are equivalent
by the symmetry of the triad of the first network form a single class,
such as `021U_102ac`, as the two edges between the top and the bottom
nodes are equivalent when both bottom nodes send a tie to the top one.

With `merge = "overlap"`, the classes of the same type of the first
network that give the same triad when both networks are overlapped are
also merged, as most groups of Figure 12 do (for instance,
`102_003-102a`: an edge of the second network on a mutual tie of the
first adds nothing to the overlapped triad).

The counts of each type of the first network add up to its triad census.

Up to version 1.0-3 the function added counts of the two networks
instead of counting the triples of each class, so its results were
wrong.

## References

Batagelj, V. and Mrvar, A. (2001). A subquadratic triad census algorithm
for large sparse networks with small maximum degree. Social Networks,
23(3), 237–243.
[doi:10.1016/S0378-8733(01)00035-1](https://doi.org/10.1016/S0378-8733%2801%2900035-1)

Espinosa-Rada, A. (2021). A Network Approach for the Sociological Study
of Science: Modelling Dynamic Multilevel Networks.
\[PhD\](https://research.manchester.ac.uk/en/studentTheses/a-network-approach-for-the-sociological-study-of-science-and-know).
The University of Manchester.

Espinosa-Rada, A., Bellotti, E., Everett, M., & Stadtfeld, C. (2024).
Co-evolution of a socio-cognitive scientific network: A case study of
citation dynamics among astronomers. Social Networks, 78, 92–108.
[doi:10.1016/j.socnet.2023.11.008](https://doi.org/10.1016/j.socnet.2023.11.008)

Holland, P. W. and Leinhardt, S. (1976). Local structure in social
networks. Sociological Methodology, 7, 1–45.
[doi:10.2307/270703](https://doi.org/10.2307/270703)

## Author

Alejandro Espinosa-Rada

## Examples

``` r

# SOAR
A <- matrix(
  c(
    0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1,
    0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1,
    0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ),
  byrow = TRUE, ncol = 12
)

B <- matrix(
  c(
    0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
    1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0,
    0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  ),
  byrow = TRUE, ncol = 12
)

multiplex_census(A, B)
#>    003_003    003_102    003_201    003_300    012_003   012_102a   012_102b 
#>         34          5          0          0         62         16          0 
#>   012_102c   012_201a   012_201c   012_201b    012_300    102_003   102_102a 
#>          1          0          0          0          0          1          4 
#>  102_102bc   102_201b  102_201ac    102_300   021D_003  021D_102b 021D_102ac 
#>          0          0          0          0         27          3          3 
#>  021D_201c 021D_201ab   021D_300   021U_003  021U_102b 021U_102ac  021U_201c 
#>          0          1          0          5          0          6          0 
#> 021U_201ab   021U_300   021C_003  021C_102a  021C_102c  021C_102b  021C_201c 
#>          0          0          9          0          3          0          0 
#>  021C_201a  021C_201b   021C_300   111D_003  111D_102b  111D_102c  111D_102a 
#>          0          0          0          2          2          0          0 
#>  111D_201b  111D_201a  111D_201c   111D_300   111U_003  111U_102b  111U_102c 
#>          0          0          0          0          2          2          0 
#>  111U_102a  111U_201b  111U_201a  111U_201c   111U_300   030T_003  030T_102b 
#>          0          0          0          0          0          4         10 
#>  030T_102a  030T_102c  030T_201a  030T_201b  030T_201c   030T_300   030C_003 
#>          1          0          0          2          0          0          0 
#>   030C_102   030C_201   030C_300    201_003   201_102c  201_102ab   201_201a 
#>          0          0          0          0          0          0          0 
#>  201_201bc    201_300   120D_003  120D_102b 120D_102ac  120D_201c 120D_201ab 
#>          0          0          0          0          1          0          1 
#>   120D_300   120U_003  120U_102b 120U_102ac  120U_201c 120U_201ab   120U_300 
#>          1          1          6          1          0          1          0 
#>   120C_003  120C_102b  120C_102c  120C_102a  120C_201b  120C_201a  120C_201c 
#>          1          1          0          0          0          0          0 
#>   120C_300    210_003   210_102b   210_102c   210_102a   210_201b   210_201a 
#>          0          0          0          0          0          0          0 
#>   210_201c    210_300    300_003    300_102    300_201    300_300 
#>          0          0          0          0          1          0 
```
