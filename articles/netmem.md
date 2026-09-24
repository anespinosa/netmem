# Getting started with netmem

`netmem` (*Network Measures using Matrices*) computes social network
measures with matrix algebra. A network is a matrix, and every function
takes and returns matrices, vectors or data frames, so the results can
be combined with the rest of `R` without converting between classes of
objects. The package imports only `Matrix` and `stats`.

This vignette shows the standard tools of social network analysis with a
single network. The vignette *What netmem adds* covers the measures that
are not available elsewhere, and the vignette *Multilayer networks*
covers two-mode, multilevel and multiplex networks.

``` r

install.packages("netmem")
```

``` r

library(netmem)
```

------------------------------------------------------------------------

## The data

The Campnet data were collected among the 18 people of a three-week
course, 14 participants and 4 instructors (Borgatti et al., 2018). At
the end of the second week, each person ranked the others by how much
they had interacted with them. `campnet$network[i, j]` is one when `j`
is among the three people with whom `i` interacted most.

``` r

data(campnet)
A <- campnet$network
gender <- campnet$attributes$gender # 1 = woman, 2 = man
role <- campnet$attributes$role # 1 = participant, 2 = instructor

matrix_report(A)
#> The matrix A might have the following characteristics:
#> --> The vectors of the matrix are `numeric`
#> --> Matrix is asymmetric (network is directed)
#> --> The matrix is square, 18 by 18
#>      nodes arcs
#> [1,]    18   54
```

Some measures are defined for undirected networks. The underlying graph
keeps a tie between two people when either of them chose the other:

``` r

U <- pmax(A, t(A))
```

------------------------------------------------------------------------

## Describing the network

Density, reciprocity and transitivity:

``` r

gen_density(A)
#> [1] 0.1764706
recip_coef(A)
#>    Mutual 
#> 0.8954248
trans_coef(A)
#> [1] 0.483871
geo_summary(A)
#> $diameter
#> [1] 7
#> 
#> $average_distance
#> [1] 2.873786
#> 
#> $prop_reachable
#> [1] 0.6732026
```

Each person chose three others, so the density is fixed by the design.
What the choices reveal is their arrangement: most of them are
reciprocated. The dyad and triad censuses show this arrangement in
detail.
[`triad_uman()`](https://anespinosa.github.io/netmem/reference/triad_uman.md)
compares each type of triad with its expectation given the number of
mutual, asymmetric and null dyads (Holland and Leinhardt, 1976):

``` r

dyadic_census(A)
#>      Mutual Asymmetrics       Nulls 
#>          19          16         118
triad_uman(A)
#>    label OBS     EXP    VAR   STD
#> 1    003 345 372.138 40.876 6.393
#> 2    012 177 153.988 57.856 7.606
#> 3    102 223 182.861 70.421 8.392
#> 4   021D   3   4.936  4.501 2.122
#> 5   021U   4   4.936  4.501 2.122
#> 6   021C   9   9.871  8.134 2.852
#> 7   111D  21  25.007 20.673 4.547
#> 8   111U   6  25.007 20.673 4.547
#> 9   030T   0   0.586  0.556 0.746
#> 10  030C   0   0.195  0.192 0.438
#> 11   201  10  28.132 17.272 4.156
#> 12  120D   5   0.795  0.776 0.881
#> 13  120U   1   0.795  0.776 0.881
#> 14  120C   0   1.589  1.516 1.231
#> 15   210   8   3.815  3.440 1.855
#> 16   300   4   1.351  1.235 1.111
```

The complete triads (`300`) and the triads with two mutual ties (`210`)
are more frequent than expected, and the open triads with two mutual
ties (`201`) less frequent: reciprocated ties tend to close.

A network can be moved between a matrix and an edge list:

``` r

E <- matrix_to_edgelist(A, digraph = TRUE)
head(E)
#>      [,1]     [,2]   
#> [1,] "HOLLY"  "PAM"  
#> [2,] "HOLLY"  "PAT"  
#> [3,] "HOLLY"  "DON"  
#> [4,] "BRAZEY" "LEE"  
#> [5,] "BRAZEY" "STEVE"
#> [6,] "BRAZEY" "BERT"
identical(edgelist_to_matrix(E, label = rownames(A)), A)
#> [1] TRUE
```

------------------------------------------------------------------------

## Centrality

The functions for centrality return a named vector, so several indices
can be gathered in a data frame. The closeness is harmonic because not
every person can be reached from every other:

``` r

centrality <- data.frame(
  indegree = gen_degree(A, type = "in"),
  closeness = closeness_centrality(A, type = "in", harmonic = TRUE),
  betweenness = betweenness_centrality(A),
  eigenvector = eigenvector_centrality(A)$vector,
  pagerank = page_rank_centrality(A)
)
round(centrality, 2)
#>         indegree closeness betweenness eigenvector pagerank
#> HOLLY          4      8.73       78.33         0.4     0.07
#> BRAZEY         1      2.92        0.00         0.0     0.02
#> CAROL          2      6.82        1.33         0.5     0.05
#> PAM            5      9.15       32.50         1.0     0.10
#> PAT            4      8.65       39.50         0.8     0.09
#> JENNIE         3      7.15        6.33         0.8     0.08
#> PAULINE        4      7.82       12.50         0.7     0.07
#> ANN            2      6.65        0.50         0.6     0.06
#> MICHAEL        4      8.15       58.83         0.1     0.05
#> BILL           0      0.00        0.00         0.0     0.01
#> LEE            3      4.33        5.00         0.0     0.06
#> DON            4      8.07       16.33         0.2     0.06
#> JOHN           0      0.00        0.00         0.0     0.01
#> HARRY          3      7.05        2.33         0.1     0.04
#> GERY           2      3.67       54.67         0.0     0.03
#> STEVE          5      5.50       16.83         0.0     0.08
#> BERT           4      5.00       13.67         0.0     0.07
#> RUSS           4      5.00       47.33         0.0     0.06
```

In a directed network, the eigenvector centrality is zero for the people
who are not reached by the chains of choices that start in the group
with the largest eigenvalue (Bonacich and Lloyd, 2001), here eight
people, among them the four instructors. PageRank avoids this with its
damping factor, which lets every person receive a small share of the
status.

The centralization of Freeman (1978) compares the network with a star of
the same size:

``` r

centrality_centralization(A, measure = "degree", digraph = TRUE, type = "in")$centralization
#> [1] 0.1323529
centrality_centralization(A, measure = "betweenness", digraph = TRUE)$centralization
#> [1] 0.4429066
```

------------------------------------------------------------------------

## Cohesive subgroups and communities

``` r

components_id(A, mode = "weak")$size
#> components
#>  1 
#> 18
k_core(U)
#>   HOLLY  BRAZEY   CAROL     PAM     PAT  JENNIE PAULINE     ANN MICHAEL    BILL 
#>       3       3       3       3       3       3       3       3       3       3 
#>     LEE     DON    JOHN   HARRY    GERY   STEVE    BERT    RUSS 
#>       3       3       3       3       3       3       3       3
clique_max(U, min = 3)
#> [[1]]
#> [1] "HOLLY"   "MICHAEL" "DON"     "HARRY"  
#> 
#> [[2]]
#> [1] "BRAZEY" "LEE"    "STEVE"  "BERT"  
#> 
#> [[3]]
#> [1] "MICHAEL" "BILL"    "DON"     "HARRY"  
#> 
#> [[4]]
#> [1] "CAROL"   "PAM"     "PAULINE"
#> 
#> [[5]]
#> [1] "CAROL"   "PAT"     "PAULINE"
#> 
#> [[6]]
#> [1] "PAM"    "JENNIE" "ANN"   
#> 
#> [[7]]
#> [1] "PAM"     "PAULINE" "ANN"    
#> 
#> [[8]]
#> [1] "JOHN" "GERY" "RUSS"
#> 
#> [[9]]
#> [1] "GERY"  "STEVE" "RUSS" 
#> 
#> [[10]]
#> [1] "STEVE" "BERT"  "RUSS"
```

The Leiden algorithm (Traag et al., 2019) finds three communities. One
has six of the eight women, another has Holly with four men who were
participants, and the third gathers the four instructors with three
participants:

``` r

set.seed(18)
communities <- leiden(U)
communities$modularity
#> [1] 0.5497959
table(community = communities$partition, gender = gender)
#>          gender
#> community 1 2
#>         1 6 0
#>         2 1 6
#>         3 1 4
table(community = communities$partition, role = role)
#>          role
#> community 1 2
#>         1 6 0
#>         2 3 4
#>         3 5 0
```

------------------------------------------------------------------------

## Homophily and positions

The mixing matrix counts the choices between the categories, and the E-I
index of Krackhardt and Stern (1988) summarises them, from -1 (every tie
within the categories) to 1 (every tie between them):

``` r

mix_matrix(A, gender)
#>     To
#> From  1  2
#>    1 20  4
#>    2  5 25
ei_index(A, att = gender)
#> [1] -0.6666667
block_density(A, gender)
#> $densities
#>           1         2
#> 1 0.3571429 0.0500000
#> 2 0.0625000 0.2777778
#> 
#> $image
#>   1 2
#> 1 1 0
#> 2 0 1
#> 
#> $density
#> [1] 0.1764706
```

The core-periphery model of Borgatti and Everett (2000):

``` r

set.seed(18)
core_periphery(U)[c("core", "periphery")]
#> $core
#> [1] "PAM"     "PAT"     "PAULINE" "MICHAEL" "DON"     "HARRY"   "GERY"   
#> [8] "STEVE"   "BERT"   
#> 
#> $periphery
#> [1] "HOLLY"  "BRAZEY" "CAROL"  "JENNIE" "ANN"    "BILL"   "LEE"    "JOHN"  
#> [9] "RUSS"
```

------------------------------------------------------------------------

## Structural holes

Effective size, efficiency and constraint of Burt (1992) for every
person. The ties are used in both directions, as in Burt (1992):

``` r

round(structural_holes(A), 2)
#>         alters effective_size efficiency constraint
#> HOLLY        5           3.86       0.77       0.44
#> BRAZEY       3           1.00       0.33       1.02
#> CAROL        3           2.00       0.67       0.80
#> PAM          5           3.88       0.78       0.52
#> PAT          4           3.57       0.89       0.39
#> JENNIE       3           2.33       0.78       0.61
#> PAULINE      5           3.86       0.77       0.54
#> ANN          3           1.60       0.53       0.93
#> MICHAEL      5           3.07       0.61       0.60
#> BILL         3           1.00       0.33       1.08
#> LEE          3           1.67       0.56       0.82
#> DON          4           2.14       0.54       0.70
#> JOHN         3           2.33       0.78       0.73
#> HARRY        4           1.75       0.44       0.79
#> GERY         4           2.90       0.73       0.65
#> STEVE        5           3.06       0.61       0.61
#> BERT         4           2.21       0.55       0.72
#> RUSS         4           2.79       0.70       0.63
```

The constraint of an ego can be split into its three terms and
normalized between the minimum and the maximum that an ego with the same
number of alters can have (Everett and Borgatti, 2020):

``` r

eb_constraint(A, ego = "HOLLY", digraph = TRUE)
#> $results
#>       term1 term2 term3 constraint normalization
#> HOLLY 0.224 0.152  0.06      0.436          0.38
#> 
#> $maximum
#> HOLLY 
#> 0.823
```

------------------------------------------------------------------------

## Statistical tests

Is the network more transitive than a random network with the same
number of ties? The conditional uniform graph test compares the observed
value with the values of random networks:

``` r

set.seed(18)
transitivity <- cug_test(A, trans_coef, cmode = "edges", reps = 500)
transitivity[c("observed", "mean", "p_greater")]
#> $observed
#> [1] 0.483871
#> 
#> $mean
#> [1] 0.1715542
#> 
#> $p_greater
#> [1] 0
```

The quadratic assignment procedure (Krackhardt, 1988) keeps the
structure of the networks and permutes the labels of the nodes. Do
people who share a gender choose each other?

``` r

same_gender <- outer(gender, gender, "==") * 1
dimnames(same_gender) <- dimnames(A)

set.seed(18)
homophily <- qap_cor(A, same_gender, reps = 500)
homophily[c("correlation", "p_greater")]
#> $correlation
#> [1] 0.3301313
#> 
#> $p_greater
#> [1] 0
```

------------------------------------------------------------------------

## How the results are checked

Each function is compared with another implementation (`igraph`, `sna`,
`netseg`, `netrankr`, `signnet`) or with the tables of the publication
that defines it. The comparisons are kept in the folder `dev/validation`
of the [GitHub repository](https://github.com/anespinosa/netmem).

------------------------------------------------------------------------

## References

Bonacich, P. and Lloyd, P. (2001). Eigenvector-like measures of
centrality for asymmetric relations. *Social Networks*, 23(3), 191–201.
<https://doi.org/10.1016/S0378-8733(01)00038-7>

Borgatti, S. P. and Everett, M. G. (2000). Models of core/periphery
structures. *Social Networks*, 21(4), 375–395.
<https://doi.org/10.1016/S0378-8733(99)00019-2>

Borgatti, S. P., Everett, M. G. and Johnson, J. C. (2018). *Analyzing
Social Networks*. Second edition. SAGE.

Burt, R. S. (1992). *Structural Holes: The Social Structure of
Competition*. Harvard University Press.

Everett, M. G. and Borgatti, S. P. (2020). Unpacking Burt’s constraint
measure. *Social Networks*, 62, 50–57.
<https://doi.org/10.1016/j.socnet.2020.02.001>

Freeman, L. C. (1978). Centrality in social networks conceptual
clarification. *Social Networks*, 1(3), 215–239.
<https://doi.org/10.1016/0378-8733(78)90021-7>

Holland, P. W. and Leinhardt, S. (1976). Local structure in social
networks. *Sociological Methodology*, 7, 1–45.
<https://doi.org/10.2307/270703>

Krackhardt, D. (1988). Predicting with networks: Nonparametric multiple
regression analysis of dyadic data. *Social Networks*, 10(4), 359–381.
<https://doi.org/10.1016/0378-8733(88)90004-4>

Krackhardt, D. and Stern, R. N. (1988). Informal networks and
organizational crises: An experimental simulation. *Social Psychology
Quarterly*, 51(2), 123–140. <https://doi.org/10.2307/2786835>

Traag, V. A., Waltman, L. and van Eck, N. J. (2019). From Louvain to
Leiden: guaranteeing well-connected communities. *Scientific Reports*,
9, 5233. <https://doi.org/10.1038/s41598-019-41695-z>
