# Segregation measures

Measures of how much the ties of a network stay within the groups given
by an attribute of the nodes, reviewed by Bojanowski and Corten (2014).

## Usage

``` r
segregation(
  A,
  att,
  method = c("assortativity", "gam", "orwg", "coleman", "freeman"),
  digraph = FALSE,
  loops = FALSE
)
```

## Arguments

- A:

  A square matrix

- att:

  A vector with the group of each node

- method:

  The measure: `assortativity` (default), `gam`, `orwg`, `coleman` or
  `freeman`

- digraph:

  Whether the matrix is directed or undirected. The `coleman` index is
  defined for directed networks

- loops:

  Whether to consider the loops of the matrix

## Value

This function returns the value of the measure, which is a value per
group for the `coleman` index.

## Details

All the measures are computed from the mixing matrix, which counts the
ties within and between groups:

`assortativity`: the proportion of ties that are within groups, compared
with the proportion expected if the ties were distributed at random
keeping how active each group is (Newman, 2003). It is one when every
tie is within a group, and zero under random mixing.

`gam`: the index of Gupta, Anderson and May (1989), the trace of the
matrix of the proportion of the ties of each group that go to every
other group, rescaled to go from \\-1/(K-1)\\ to one. It is defined for
undirected networks, and every group should have at least one tie.

`orwg`: the odds of a tie within a group divided by the odds of a tie
between groups (Moody, 2001). Unlike the other measures, it takes into
account the pairs of nodes that are not tied, so it is not affected by
the density of the network.

`coleman`: the homophily index of Coleman (1958), computed for each
group: how many ties the group sends to itself compared with the ties it
would send if it chose the other nodes at random. It is one when the
group only relates to itself.

`freeman`: the segregation index of Freeman (1978) for two groups: how
many fewer ties between the groups there are than the ones expected in a
random network with the same density and group sizes. It is zero when
there are as many as expected, or more.

## References

Bojanowski, M. and Corten, R. (2014). Measuring segregation in social
networks. Social Networks, 39, 14–32.
[doi:10.1016/j.socnet.2014.04.001](https://doi.org/10.1016/j.socnet.2014.04.001)

Coleman, J. (1958). Relational analysis: The study of social
organizations with survey methods. Human Organization, 17(4), 28–36.
[doi:10.17730/humo.17.4.q5604m676260q8n7](https://doi.org/10.17730/humo.17.4.q5604m676260q8n7)

Freeman, L. C. (1978). Segregation in social networks. Sociological
Methods and Research, 6(4), 411–429.
[doi:10.1177/004912417800600401](https://doi.org/10.1177/004912417800600401)

Gupta, S., Anderson, R. M. and May, R. M. (1989). Networks of sexual
contacts: implications for the pattern of spread of HIV. AIDS, 3(12),
807–817.
[doi:10.1097/00002030-198912000-00005](https://doi.org/10.1097/00002030-198912000-00005)

Moody, J. (2001). Race, school integration, and friendship segregation
in America. American Journal of Sociology, 107(3), 679–716.
[doi:10.1086/338954](https://doi.org/10.1086/338954)

Newman, M. E. J. (2003). Mixing patterns in networks. Physical Review E,
67(2), 026126.
[doi:10.1103/PhysRevE.67.026126](https://doi.org/10.1103/PhysRevE.67.026126)

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
att <- c("a", "a", "a", "b", "b", "b")

segregation(A, att)
#> [1] 0.7142857
segregation(A, att, method = "orwg")
#> [1] Inf
segregation(A, att, method = "coleman", digraph = FALSE)
#>         a         b 
#> 0.7619048 0.7619048 
```
