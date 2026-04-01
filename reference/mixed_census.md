# Multilevel triad and quadrilateral census

Multilevel triad and quadrilateral census

## Usage

``` r
mixed_census(A1, B1, B2 = NULL, quad = FALSE)
```

## Arguments

- A1:

  An adjacent matrix object.

- B1:

  An incidence matrix object.

- B2:

  An incidence matrix object.

- quad:

  Whether the matrix is a quadrilateral census or not.

## Value

This function return the counts of a multilevel census.

If `quad = TRUE`, then the function return the multilevel quadrilateral
census.

## References

Espinosa-Rada, A. (2021). A Network Approach for the Sociological Study
of Science: Modelling Dynamic Multilevel Networks.
\[PhD\](https://research.manchester.ac.uk/en/studentTheses/a-network-approach-for-the-sociological-study-of-science-and-know).
The University of Manchester.

Espinosa-Rada, A., Bellotti, E., Everett, M., & Stadtfeld, C. (2024).
Co-evolution of a socio-cognitive scientific network: A case study of
citation dynamics among astronomers. Social Networks, 78, 92–108.
[doi:10.1016/j.socnet.2023.11.008](https://doi.org/10.1016/j.socnet.2023.11.008)

Hollway, J., Lomi, A., Pallotti, F., & Stadtfeld, C. (2017). Multilevel
social spaces: The network dynamics of organizational fields. Network
Science, 5(2), 187–212.
[doi:10.1017/nws.2017.8](https://doi.org/10.1017/nws.2017.8)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
B1 <- matrix(c(
  1, 1, 0,
  0, 0, 1,
  0, 0, 1,
  1, 0, 0
), byrow = TRUE, ncol = 3)
A1 <- matrix(c(
  0, 1, 0, 1,
  1, 0, 0, 1,
  0, 1, 0, 1,
  1, 0, 1, 0
), byrow = TRUE, ncol = 4)
B2 <- matrix(c(
  1, 0, 0, 0, 0,
  0, 1, 0, 1, 0,
  0, 0, 0, 0, 0,
  0, 0, 0, 0, 0
), byrow = TRUE, ncol = 5)

mixed_census(A1, B1, B2, quad = TRUE)
#>   000   100   001   010   020   200  11D0  11U0   120   210   220   002  01D1 
#>     0    12     0     9     9     0     3     3    20     3     4     0     4 
#>  01U1   012   021   022  101N  101P   201   102   202 11D1W 11U1P 11D1P 11U1W 
#>     2     0     1     0     1     2     0     0     0     2     0     0     2 
#>  121W  121P  21D1  21U1  11D2  11U2   221   122   212   222 
#>     5     5     0     2     0     0     1     0     0     0 
```
