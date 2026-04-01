# Multiplex triad census

This function counts the different subgraphs of three nodes in a
multiplex directed and undirected network.

## Usage

``` r
multiplex_census(A, B)
```

## Arguments

- A:

  A directed matrix object.

- B:

  An undirected matrix object.

## Value

This function gives the counts of the mixed multiplex triad census for a
directed and an undirected network.

## References

Espinosa-Rada, A. (2021). A Network Approach for the Sociological Study
of Science: Modelling Dynamic Multilevel Networks.
\[PhD\](https://research.manchester.ac.uk/en/studentTheses/a-network-approach-for-the-sociological-study-of-science-and-know).
The University of Manchester.

Espinosa-Rada, A., Bellotti, E., Everett, M., & Stadtfeld, C. (2024).
Co-evolution of a socio-cognitive scientific network: A case study of
citation dynamics among astronomers. Social Networks, 78, 92–108.
[doi:10.1016/j.socnet.2023.11.008](https://doi.org/10.1016/j.socnet.2023.11.008)

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
#>            003_003            003_102            003_201            003_300 
#>                 17                 52                 22                 20 
#>            012_003           012_102a           012_102b           012_102c 
#>                113                 52                 43                 46 
#>           012_201b          012_201ac            012_300           021u_003 
#>                 43                 39                 40                 79 
#>         021u_102ac          021u_102b         021u_201ab          021u_201c 
#>                 12                 14                  9                  5 
#>           021u_300           021d_003         021d_102ac          021d_120b 
#>                  6                 91                 20                 18 
#>         021d_201ab          021d_201c           021d_300       102_003_102a 
#>                 20                 17                 17                 15 
#>    102_102bc_201ac            102_300           021c_003          021c_102a 
#>                  2                  3                 80                  9 
#>          021c_103b          021c_102c         021c_210ab          021c_201c 
#>                  7                 13                  9                  6 
#>           021c_300           030t_003         030t_102ab          030t_102b 
#>                  7                 82                 10                 17 
#>          030t_102c         030t_210ab      030t_201c_300           030c_003 
#>                 10                 12                  9                 74 
#>        030c_102abc        030c_201abc           030c_300           111d_003 
#>                  1                  3                  1                 76 
#>     111d_102a_201a          111d_102b     111d_102c_201b      111d_201c_300 
#>                  5                  7                  2                  3 
#>           111u_003     111u_102a_201a    111u_102bc_201b      111u_201c_300 
#>                 76                  5                  2                  3 
#>      120u_003_102b   120u_102ab_201ab      120u_201c_300      120d_003_120b 
#>                 13                  8                  5                  3 
#>   120d_102ab_201ab      120d_201c_300            201_003    201_102ac_201ab 
#>                  5                  2                 74                  0 
#> 201_102c_201bc_300           120c_003          120c_120c           120c_210 
#>                  1                 75                  2                  4 
#>           120c_300        210_003_210            210_300        300_003_300 
#>                  2                  3                  1                  1 
```
