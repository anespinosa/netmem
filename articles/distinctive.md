# What netmem adds

Besides the standard measures (see *Getting started with netmem*),
`netmem` implements measures that are hard to find in other packages,
several of them proposed in the last few years. Each one is compared
with the tables of the publication that defines it, and the comparisons
are kept in the folder `dev/validation` of the [GitHub
repository](https://github.com/anespinosa/netmem).

This vignette uses the Campnet network: the three people with whom each
of the 18 people of a course interacted most (Borgatti et al., 2018).

``` r

library(netmem)

data(campnet)
A <- campnet$network
U <- pmax(A, t(A)) # Underlying graph
gender <- campnet$attributes$gender # 1 = woman, 2 = man
```

------------------------------------------------------------------------

## Ranking without choosing a centrality index

Every centrality index gives a ranking, and the rankings of different
indices often disagree. Schoch and Brandes (2016) show what they all
share: when the neighbours of `u` are also neighbours of `v`, every
standard index ranks `v` at least as high as `u`. This *neighbourhood
inclusion* is a partial ranking, implied by the structure of the network
before choosing any index.

``` r

P <- neigh_inclusion(U) # P[u, v] = 1 when u is dominated by v
dominance_pairs(P)[c("comparable", "incomparable", "prop_comparable")]
#> $comparable
#> [1] 17
#> 
#> $incomparable
#> [1] 136
#> 
#> $prop_comparable
#> [1] 0.1111111
```

Only 11% of the pairs of people are ranked by the structure itself. For
the other 89%, the order depends on the index chosen. The rank of each
person is an interval, from the lowest (one) to the highest rank that
the person can take in a ranking consistent with the partial ranking. A
wide interval means that the position of the person depends on the
index:

``` r

dominance_ranks(P)
#>       node min_rank max_rank width
#> 1    HOLLY        2       18    16
#> 2   BRAZEY        1       15    14
#> 3    CAROL        1       17    16
#> 4      PAM        3       18    15
#> 5      PAT        1       17    16
#> 6   JENNIE        1       17    16
#> 7  PAULINE        3       18    15
#> 8      ANN        1       17    16
#> 9  MICHAEL        4       18    14
#> 10    BILL        1       14    13
#> 11     LEE        1       15    14
#> 12     DON        2       16    14
#> 13    JOHN        1       18    17
#> 14   HARRY        2       16    14
#> 15    GERY        1       18    17
#> 16   STEVE        4       18    14
#> 17    BERT        3       17    14
#> 18    RUSS        1       18    17
```

Two people with the same neighbours dominate each other. Removing these
ties gives the strict dominance, whose layers go from the people who are
not dominated by anyone to the most dominated:

``` r

strict <- P * (1 - t(P))
dominance_layers(strict)$layers
#> [[1]]
#> [1] "HOLLY"   "PAM"     "PAULINE" "MICHAEL" "JOHN"    "GERY"    "STEVE"  
#> [8] "RUSS"   
#> 
#> [[2]]
#> [1] "CAROL"  "PAT"    "JENNIE" "ANN"    "DON"    "HARRY"  "BERT"  
#> 
#> [[3]]
#> [1] "BRAZEY" "BILL"   "LEE"
```

[`preserved_order()`](https://anespinosa.github.io/netmem/reference/preserved_order.md)
checks whether an index respects the partial ranking:

``` r

preserved_order(P, betweenness_centrality(U, digraph = FALSE))$preserved
#> [1] TRUE
```

In directed networks, Marmulla and Brandes (2026) show that each family
of indices preserves a different criterion. The indices of status, such
as in-degree and PageRank, preserve the inclusion of the choices
received (`radial_in`), whereas betweenness does not:

``` r

D <- dir_inclusion(A, type = "radial_in")
preserved_order(D, colSums(A))$preserved
#> [1] TRUE
preserved_order(D, page_rank_centrality(A))$preserved
#> [1] TRUE
preserved_order(D, betweenness_centrality(A))
#> $preserved
#> [1] FALSE
#> 
#> $violations
#>   dominated dominating score_dominated score_dominating
#> 1       PAT        PAM            39.5             32.5
```

Pam receives the choices of everyone who chooses Pat, and more, yet Pat
is more central than Pam by betweenness.

------------------------------------------------------------------------

## Overlapping categories

The measures of homophily, brokerage and structural holes assume that
each person belongs to one category. Everett and Borgatti (2026)
generalise them to memberships that overlap, such as groups, cliques or
the time spent in several activities. Here the categories are the ten
maximal cliques of the underlying graph, and several people belong to
more than one:

``` r

cliques <- clique_max(U, min = 3)
K <- matrix(0, nrow(U), length(cliques),
  dimnames = list(rownames(U), paste0("C", seq_along(cliques)))
)
for (k in seq_along(cliques)) {
  K[cliques[[k]], k] <- 1
}
K
#>         C1 C2 C3 C4 C5 C6 C7 C8 C9 C10
#> HOLLY    1  0  0  0  0  0  0  0  0   0
#> BRAZEY   0  1  0  0  0  0  0  0  0   0
#> CAROL    0  0  0  1  1  0  0  0  0   0
#> PAM      0  0  0  1  0  1  1  0  0   0
#> PAT      0  0  0  0  1  0  0  0  0   0
#> JENNIE   0  0  0  0  0  1  0  0  0   0
#> PAULINE  0  0  0  1  1  0  1  0  0   0
#> ANN      0  0  0  0  0  1  1  0  0   0
#> MICHAEL  1  0  1  0  0  0  0  0  0   0
#> BILL     0  0  1  0  0  0  0  0  0   0
#> LEE      0  1  0  0  0  0  0  0  0   0
#> DON      1  0  1  0  0  0  0  0  0   0
#> JOHN     0  0  0  0  0  0  0  1  0   0
#> HARRY    1  0  1  0  0  0  0  0  0   0
#> GERY     0  0  0  0  0  0  0  1  1   0
#> STEVE    0  1  0  0  0  0  0  0  1   1
#> BERT     0  1  0  0  0  0  0  0  0   1
#> RUSS     0  0  0  0  0  0  0  1  1   1
```

Each membership is divided by the number of categories of the person, so
that every person counts once. The composition of the alters of each
person gives how many of them, fractionally, belong to each clique, and
the heterogeneity summarises it:

``` r

round(alter_composition(A, K), 2)
#>          C1   C2  C3   C4   C5   C6   C7   C8   C9  C10
#> HOLLY   0.5 0.00 0.5 0.33 1.00 0.33 0.33 0.00 0.00 0.00
#> BRAZEY  0.0 1.83 0.0 0.00 0.00 0.00 0.00 0.00 0.33 0.83
#> CAROL   0.0 0.00 0.0 0.67 1.33 0.33 0.67 0.00 0.00 0.00
#> PAM     0.0 0.00 0.0 0.33 0.33 1.50 0.83 0.00 0.00 0.00
#> PAT     1.0 0.00 0.0 0.50 0.50 1.00 0.00 0.00 0.00 0.00
#> JENNIE  0.0 0.00 0.0 0.33 1.00 0.83 0.83 0.00 0.00 0.00
#> PAULINE 0.0 0.00 0.0 0.83 1.50 0.33 0.33 0.00 0.00 0.00
#> ANN     0.0 0.00 0.0 0.67 0.33 1.33 0.67 0.00 0.00 0.00
#> MICHAEL 2.0 0.00 1.0 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> BILL    1.5 0.00 1.5 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> LEE     0.0 1.83 0.0 0.00 0.00 0.00 0.00 0.00 0.33 0.83
#> DON     2.0 0.00 1.0 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> JOHN    0.0 0.00 0.0 0.33 0.33 0.00 0.33 0.83 0.83 0.33
#> HARRY   2.0 0.00 1.0 0.00 0.00 0.00 0.00 0.00 0.00 0.00
#> GERY    0.5 0.33 0.5 0.00 0.00 0.00 0.00 0.33 0.67 0.67
#> STEVE   0.0 1.50 0.0 0.00 0.00 0.00 0.00 0.33 0.33 0.83
#> BERT    0.0 1.33 0.0 0.00 0.00 0.00 0.00 0.33 0.67 0.67
#> RUSS    0.0 0.83 0.0 0.00 0.00 0.00 0.00 0.50 0.83 0.83
round(alter_heterogeneity(A, K), 2)
#>   HOLLY  BRAZEY   CAROL     PAM     PAT  JENNIE PAULINE     ANN MICHAEL    BILL 
#>    0.80    0.54    0.69    0.65    0.72    0.72    0.65    0.69    0.44    0.50 
#>     LEE     DON    JOHN   HARRY    GERY   STEVE    BERT    RUSS 
#>    0.54    0.44    0.80    0.44    0.82    0.65    0.69    0.74
```

The E-I index and Yule’s Q with overlapping categories:

``` r

round(cbind(
  ei = alter_homophily(A, K),
  yule = alter_homophily(A, K, method = "yule")
), 2)
#>            ei yule
#> HOLLY    0.67 0.44
#> BRAZEY  -0.22 1.00
#> CAROL    0.33 1.00
#> PAM      0.41 0.94
#> PAT      0.67 0.78
#> JENNIE   0.44 1.00
#> PAULINE  0.41 0.94
#> ANN      0.33 1.00
#> MICHAEL  0.00 0.93
#> BILL     0.00 1.00
#> LEE     -0.22 1.00
#> DON      0.00 0.93
#> JOHN     0.44 1.00
#> HARRY    0.00 0.93
#> GERY     0.67 0.69
#> STEVE    0.41 0.84
#> BERT     0.33 0.86
#> RUSS     0.52 0.86
```

The brokerage roles of Gould and Fernandez (1989) become fractional, as
each broker, sender and receiver might share several categories:

``` r

round(brokerage_roles(A, K), 2)
#>         coordinator gatekeeper representative consultant liaison total
#> HOLLY          0.00       0.50           3.00       0.00    4.50     8
#> BRAZEY         0.00       0.00           0.00       0.00    0.00     0
#> CAROL          0.17       0.33           0.83       0.17    0.50     2
#> PAM            0.06       2.39           1.06       0.11    4.39     8
#> PAT            0.00       1.00           1.67       0.00    5.33     8
#> JENNIE         0.00       0.83           0.83       0.00    2.33     4
#> PAULINE        0.06       2.17           0.72       0.11    3.94     7
#> ANN            0.00       0.17           0.50       0.00    0.33     1
#> MICHAEL        0.00       2.00           0.50       0.00    1.50     4
#> BILL           0.00       0.00           0.00       0.00    0.00     0
#> LEE            0.83       1.17           0.00       0.00    0.00     2
#> DON            0.50       1.00           1.00       0.50    0.00     3
#> JOHN           0.00       0.00           0.00       0.00    0.00     0
#> HARRY          0.00       0.50           0.50       0.00    0.00     1
#> GERY           0.00       0.17           1.33       0.00    1.50     3
#> STEVE          0.00       1.44           1.22       0.00    2.33     5
#> BERT           0.00       0.83           1.17       0.00    1.00     3
#> RUSS           0.06       1.17           1.33       0.11    2.33     5
```

Betweenness can be split by the category of the people who need the
brokers to reach the others. The column sums give how much the members
of each clique depend on people in between (Everett and Borgatti, 2026:
Table 6). The clique of Brazey, Lee, Steve and Bert (`C2`) depends on
them the most:

``` r

round(colSums(partition_centrality(A, K)), 1)
#>    C1    C2    C3    C4    C5    C6    C7    C8    C9   C10 
#>  21.0 138.2  32.0  14.2  17.2  23.5  16.2  43.2  35.8  44.8
```

Finally, two alters of the same category might give access to the same
information even when they are not tied.
[`structural_holes()`](https://anespinosa.github.io/netmem/reference/structural_holes.md)
adds a tie of strength `beta` between them. With gender as the category,
the effective size falls most for Pam, Gery and Pat, whose alters are of
the same gender but not tied to each other:

``` r

holes <- data.frame(
  gender = gender,
  original = structural_holes(A)$effective_size,
  same_gender = structural_holes(A, gender, beta = 0.5)$effective_size,
  row.names = rownames(A)
)
round(holes, 2)
#>         gender original same_gender
#> HOLLY        1     3.86        3.50
#> BRAZEY       1     1.00        1.00
#> CAROL        1     2.00        1.50
#> PAM          1     3.88        2.00
#> PAT          1     3.57        2.29
#> JENNIE       1     2.33        1.67
#> PAULINE      1     3.86        3.14
#> ANN          1     1.60        1.30
#> MICHAEL      2     3.07        2.05
#> BILL         2     1.00        1.00
#> LEE          2     1.67        1.67
#> DON          2     2.14        2.07
#> JOHN         2     2.33        2.33
#> HARRY        2     1.75        1.67
#> GERY         2     2.90        1.48
#> STEVE        2     3.06        2.44
#> BERT         2     2.21        1.93
#> RUSS         2     2.79        1.80
```

------------------------------------------------------------------------

## Q-analysis

The Q-analysis of Atkin (1974) describes a network through its maximal
cliques (simplices) and how they share nodes. Two cliques are
*q*-connected when a chain of cliques joins them, each sharing at least
*q* + 1 nodes with the next. Freeman (1980) used it to study the
structure of friendship networks.

``` r

q <- q_analysis(U)
q$q_table
#>   q Q  n      Qbar obstruction
#> 1 3 3  3 0.0000000           2
#> 2 2 9 10 0.1000000           8
#> 3 1 8 15 0.4666667           7
#> 4 0 1 15 0.9333333           0
q$components$q1
#>    component                 simplex
#> 1          1 HOLLY-MICHAEL-DON-HARRY
#> 3          1  MICHAEL-BILL-DON-HARRY
#> 2          2   BRAZEY-LEE-STEVE-BERT
#> 8          2          JOHN-GERY-RUSS
#> 9          2         GERY-STEVE-RUSS
#> 10         2         STEVE-BERT-RUSS
#> 4          3       CAROL-PAM-PAULINE
#> 5          3       CAROL-PAT-PAULINE
#> 6          3          PAM-JENNIE-ANN
#> 7          3         PAM-PAULINE-ANN
#> 11         4               HOLLY-PAM
#> 12         5               HOLLY-PAT
#> 13         6              PAT-JENNIE
#> 14         7            PAULINE-JOHN
#> 15         8            MICHAEL-GERY
```

At *q* = 0 the whole network is connected, at *q* = 1 the cliques of the
instructors join those of Brazey and Lee, and at *q* = 3 only the three
cliques of four people remain. The eccentricity measures how much a
clique stands apart from the rest:

``` r

q$eccentricity
#>                    simplex dimension bottom eccentricity
#> 1  HOLLY-MICHAEL-DON-HARRY         3      2    0.3333333
#> 2    BRAZEY-LEE-STEVE-BERT         3      1    1.0000000
#> 3   MICHAEL-BILL-DON-HARRY         3      2    0.3333333
#> 4        CAROL-PAM-PAULINE         2      1    0.5000000
#> 5        CAROL-PAT-PAULINE         2      1    0.5000000
#> 6           PAM-JENNIE-ANN         2      1    0.5000000
#> 7          PAM-PAULINE-ANN         2      1    0.5000000
#> 8           JOHN-GERY-RUSS         2      1    0.5000000
#> 9          GERY-STEVE-RUSS         2      1    0.5000000
#> 10         STEVE-BERT-RUSS         2      1    0.5000000
#> 11               HOLLY-PAM         1      0    1.0000000
#> 12               HOLLY-PAT         1      0    1.0000000
#> 13              PAT-JENNIE         1      0    1.0000000
#> 14            PAULINE-JOHN         1      0    1.0000000
#> 15            MICHAEL-GERY         1      0    1.0000000
```

------------------------------------------------------------------------

## Citation networks

A small corpus of 13 papers written by six authors, where
`cites[p, q] = 1` when paper `p` cites paper `q` (the network of Kuan,
2020: Fig. 2):

``` r

papers <- paste0("p", 1:13)
references <- list(
  p4 = c("p1", "p2", "p3"), p5 = "p4", p6 = "p4", p7 = "p5",
  p8 = c("p6", "p7"), p9 = "p7", p10 = "p7", p11 = "p7", p12 = "p8", p13 = "p8"
)
cites <- matrix(0, 13, 13, dimnames = list(papers, papers))
for (p in names(references)) {
  cites[p, references[[p]]] <- 1
}

authors <- list(
  p1 = "Ada", p2 = "Bo", p3 = c("Ada", "Cy"), p4 = c("Ada", "Bo"), p5 = "Cy",
  p6 = c("Bo", "Di"), p7 = c("Cy", "Ed"), p8 = "Di", p9 = "Ed", p10 = c("Ed", "Flo"),
  p11 = "Flo", p12 = c("Di", "Flo"), p13 = c("Ada", "Di")
)
X <- matrix(0, 6, 13, dimnames = list(c("Ada", "Bo", "Cy", "Di", "Ed", "Flo"), papers))
for (p in names(authors)) {
  X[authors[[p]], p] <- 1
}
```

### Main path analysis

Main path analysis follows the flow of knowledge, from the cited paper
to the citing one (Hummon and Doreian, 1989), so it uses the transpose
of `cites`. The traversal weights count how many paths between the first
and the last papers go through each citation:

``` r

flow <- t(cites)
dag_check(flow)$is_dag
#> [1] TRUE

spc <- traversal_weights(flow, method = "spc")
matrix_to_edgelist(spc$edge_weights, digraph = TRUE, valued = TRUE)
#>       [,1] [,2]  [,3]
#>  [1,] "p1" "p4"  "7" 
#>  [2,] "p2" "p4"  "7" 
#>  [3,] "p3" "p4"  "7" 
#>  [4,] "p4" "p5"  "15"
#>  [5,] "p4" "p6"  "6" 
#>  [6,] "p5" "p7"  "15"
#>  [7,] "p6" "p8"  "6" 
#>  [8,] "p7" "p8"  "6" 
#>  [9,] "p7" "p9"  "3" 
#> [10,] "p7" "p10" "3" 
#> [11,] "p7" "p11" "3" 
#> [12,] "p8" "p12" "6" 
#> [13,] "p8" "p13" "6"
```

The global main path is the route with the largest total weight, and the
key-route search starts from the arcs with the largest weights (Liu and
Lu, 2012):

``` r

main_path(flow, method = "global")$routes
#> [[1]]
#> [1] "p1"  "p4"  "p5"  "p7"  "p8"  "p12"
main_path(flow, method = "key_route", k = 2)$routes
#> [[1]]
#> [1] "p1"  "p4"  "p5"  "p7"  "p8"  "p12"
#> 
#> [[2]]
#> [1] "p2"  "p4"  "p5"  "p7"  "p8"  "p12"
```

The weights SPLC and SPNP (`method = "splc"`, `"spnp"`) and the
diagnostics of
[`main_path_diag()`](https://anespinosa.github.io/netmem/reference/main_path_diag.md)
follow Liu et al. (2019) and Kuan (2020).

### Fractional counting

When the citations between papers are aggregated to citations between
authors, full counting gives each coauthor of a paper the whole
citation, so the total grows with the size of the teams. Fractional
counting divides each citation among the authors, and the total remains
the number of citations (Batagelj, 2020):

``` r

fractional_approach(cites, t(X), fractional = FALSE)
#>     Ada Bo Cy Di Ed Flo
#> Ada   2  1  1  1  0   0
#> Bo    3  2  1  0  0   0
#> Cy    1  1  1  0  0   0
#> Di    1  2  1  3  1   0
#> Ed    0  0  3  0  2   0
#> Flo   0  0  2  1  2   0
round(fractional_approach(cites, t(X)), 2)
#>      Ada   Bo   Cy  Di   Ed Flo
#> Ada 0.75 0.50 0.25 0.5 0.00   0
#> Bo  1.00 0.75 0.25 0.0 0.00   0
#> Cy  0.50 0.50 0.50 0.0 0.00   0
#> Di  0.25 0.75 0.50 1.5 0.50   0
#> Ed  0.00 0.00 1.25 0.0 0.75   0
#> Flo 0.00 0.00 0.75 0.5 0.75   0
sum(fractional_approach(cites, t(X)))
#> [1] 13
sum(cites)
#> [1] 13
```

The fractional bibliographic coupling of two papers is not symmetric,
and it can be made symmetric with one of six measures (here the
geometric mean, that is, Salton’s cosine):

``` r

coupling <- fractional_approach(cites, approach = "bcoupling", symmetric = "geometric")
round(coupling[c("p8", "p9", "p10"), c("p8", "p9", "p10")], 2)
#>       p8   p9  p10
#> p8  1.00 0.71 0.71
#> p9  0.71 1.00 1.00
#> p10 0.71 1.00 1.00
```

### Dominance among authors

The hyper-event dominance (Espinosa-Rada, 2026) compares authors through
the chain author, citing paper, cited paper, cited author, in three
dimensions: the papers written, the papers cited and the authors cited.
An author dominates another when the neighbourhood of the second is
included in that of the first in at least `tau` dimensions:

``` r

H <- hyperevent_dominance(X, cites, tau = 2) # H[u, v] = 1 when u is dominated by v
H
#>     Ada Bo Cy Di Ed Flo
#> Ada   0  0  0  0  0   0
#> Bo    1  0  0  0  0   0
#> Cy    0  0  0  0  0   0
#> Di    0  0  0  0  0   0
#> Ed    0  0  0  0  0   0
#> Flo   0  0  0  1  0   0
dominance_layers(H)$status
#>           Ada            Bo            Cy            Di            Ed 
#>    "dominant"   "dominated" "independent"    "dominant" "independent" 
#>           Flo 
#>   "dominated"
```

Ada dominates Bo and Di dominates Flo, while Cy and Ed are not
comparable with anyone.

------------------------------------------------------------------------

## Multilevel and multiplex networks

The vignette *Multilayer networks* shows the functions for networks with
several levels or several relations: the meta-matrix, the degree and
*k*-core of multilevel networks, the mixed triad census of a network and
a two-mode network, and the triad census of a directed and an undirected
relation among the same people (Espinosa-Rada et al., 2024).

------------------------------------------------------------------------

## References

Atkin, R. H. (1974). *Mathematical Structure in Human Affairs*. Crane,
Russak.

Batagelj, V. (2020). On fractional approach to analysis of linked
networks. *Scientometrics*, 123(2), 621–633.
<https://doi.org/10.1007/s11192-020-03383-y>

Borgatti, S. P., Everett, M. G. and Johnson, J. C. (2018). *Analyzing
Social Networks*. Second edition. SAGE.

Espinosa-Rada, A. (2026). Network positions within scholars and
intellectual networks. *Journal of Informetrics*, 20(3), 101854.
<https://doi.org/10.1016/j.joi.2026.101854>

Espinosa-Rada, A., Bellotti, E., Everett, M. and Stadtfeld, C. (2024).
Co-evolution of a socio-cognitive scientific network: A case study of
citation dynamics among astronomers. *Social Networks*, 78, 92–108.
<https://doi.org/10.1016/j.socnet.2023.11.008>

Everett, M. G. and Borgatti, S. P. (2026). Alter composition with
overlapping group memberships. *Social Networks*, 85, 80–88.
<https://doi.org/10.1016/j.socnet.2025.12.001>

Freeman, L. C. (1980). Q-analysis and the structure of friendship
networks. *International Journal of Man-Machine Studies*, 12(4),
367–378. <https://doi.org/10.1016/S0020-7373(80)80021-6>

Gould, R. V. and Fernandez, R. M. (1989). Structures of mediation: A
formal approach to brokerage in transaction networks. *Sociological
Methodology*, 19, 89–126. <https://doi.org/10.2307/270949>

Hummon, N. P. and Doreian, P. (1989). Connectivity in a citation
network: The development of DNA theory. *Social Networks*, 11(1), 39–63.
<https://doi.org/10.1016/0378-8733(89)90017-8>

Kuan, C. H. (2020). Regarding weight assignment algorithms of main path
analysis and the conversion of arc weights to node weights.
*Scientometrics*, 124(1), 775–782.
<https://doi.org/10.1007/s11192-020-03468-8>

Liu, J. S. and Lu, L. Y. Y. (2012). An integrated approach for main path
analysis: Development of the Hirsch index as an example. *Journal of the
American Society for Information Science and Technology*, 63(3),
528–542. <https://doi.org/10.1002/asi.21692>

Liu, J. S., Lu, L. Y. Y. and Ho, M. H. C. (2019). A few notes on main
path analysis. *Scientometrics*, 119(1), 379–391.
<https://doi.org/10.1007/s11192-019-03034-x>

Marmulla, G. and Brandes, U. (2026). Centrality in directed networks.
*Social Networks*, 86, 23–34.
<https://doi.org/10.1016/j.socnet.2026.01.001>

Schoch, D. and Brandes, U. (2016). Re-conceptualizing centrality in
social networks. *European Journal of Applied Mathematics*, 27(6),
971–985. <https://doi.org/10.1017/S0956792516000401>
