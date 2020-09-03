
<!-- README.md is generated from README.Rmd. Please edit that file -->

netmem: Network Measures using Matrices
=======================================

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/netmem)](https://CRAN.R-project.org/package=netmem)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `netmem` is to make available different measures to describe
networks using matrices.

🖊 Author/mantainer: [Alejandro
Espinosa-Rada](https://www.research.manchester.ac.uk/portal/en/researchers/alejandro-espinosa(4ed72800-e02b-47a8-a958-640b6a07f563).html)

🏫 [The Mitchell Centre for Social Network
Analysis](https://www.socialsciences.manchester.ac.uk/mitchell-centre/),
The University of Manchester

[![Follow me on
Twitter](https://img.shields.io/badge/Follow%20me%20on%20Twitter-9cf.svg)](https://twitter.com/aesp08)

The package implements different measures to analyse complex multilayer
networks, from an ego-centric perspective, considering one-mode
networks, valued ties (i.e. *weighted* or *multiplex*) or with multiple
levels.

Installation
------------

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("anespinosa/netmem")

    library(netmem)

------------------------------------------------------------------------

Multilevel Networks
-------------------

Connections between individuals are often embedded in complex
structures, which shape actors’ expectations, behaviours and outcomes
over time. These structures can themselves be interdependent and exist
at different levels. Multilevel networks are a means by which we can
represent this complex system by using nodes and edges of different
types. Check [this book](https://www.springer.com/gp/book/9783319245188)
edited by Emmanuel Lazega and Tom A.B. Snijders.

<img src="man/figures/multilevel.png"/>

For multilevel structures, we tend to collect the data in different
matrices representing the variation of ties within and between levels.
Often, we describe the connection between actors as an adjacency matrix
and the relations between levels through incident matrices. The
comfortable combination of these matrices into a common structure would
represent the multilevel network that could be highly complex.

### Example

<div class="alert alert-info">

Let’s assume that we have a multilevel network with two adjacency
matrices, one valued matrix and two incident matrices between them.

-   `A1`: Adjacency Matrix of the level 1
-   `B1`: Incident Matrix between level 1 and level 2
-   `A2`: Adjacency Matrix of the level 2
-   `B2`: Incident Matrix between level 2 and level 3
-   `A3`: Valued Matrix of the level 3
-   `B3`: Incident Matrix between level 3 and level 1

</div>

Create the data

    A1 <- matrix(c(0,1,0,0,1,
                  1,0,0,1,1,
                  0,0,0,1,1,
                  0,1,1,0,1,
                  1,1,1,1,0), byrow=TRUE, ncol=5)
                  
    B1 <- matrix(c(1,0,0,
                  1,1,0,
                  0,1,0,
                  0,1,0,
                  0,1,1), byrow=TRUE, ncol=3)

    A2 <- matrix(c(0,1,1,
                  1,0,0,
                  1,0,0), byrow=TRUE, nrow=3)

    B2 <- matrix(c(1,1,0,0,
                  0,0,1,0,
                  0,0,1,1), byrow=TRUE, ncol=4)

    A3 <- matrix(c(0,1,3,1,
                  1,0,0,0,
                  3,0,0,5,
                  1,0,5,0), byrow=TRUE, ncol=4)

    B3 <- matrix(c(1,0,0,0,0,
                  0,1,0,1,0,
                  0,0,0,0,0,
                  0,0,0,0,0), byrow=TRUE, ncol=5)

------------------------------------------------------------------------

### Ego measures

When we are interested in one particular actor, we could perform
different network measures. For example, actor `5` has connections with
all the other actors in the network. Therefore, we could estimate its
constraint.

    eb_constraint(A1, ego=5)
    #> $results
    #>   term1 term2 term3 constraint normalization
    #> 1  0.25 0.292 0.101      0.642         0.761
    #> 
    #> $maximum
    #> [1] 0.766

------------------------------------------------------------------------

### One-mode network

This package implements the generalized degree centrality. Suppose we
consider a valued matrix `A3`. If `alpha=0` then it would only count the
direct connections. But, adding the tuning parameter `alpha=0.5` would
determine the relative importance of the number of ties compared to tie
weights.

    gen_degree(A3, digraph = FALSE, weighted=TRUE)
    #> [1] 3.872983 1.000000 4.000000 3.464102

Also, we could conduct some explorative analysis using the normalized
degree of an incident matrix.

    gen_degree(B1, bipartite = TRUE, normalized=TRUE)
    #> $bipartiteL1
    #> [1] 0.3333333 0.6666667 0.3333333 0.3333333 0.6666667
    #> 
    #> $bipartiteL2
    #> [1] 0.4 0.8 0.2

This package also implement some dyadic analysis

    # Dyadic census
    dyadic_census(A1)
    #>      Mutual Asymmetrics       Nulls 
    #>           7           0           3

    # Katz and Powell reciprocity
    pkp(A1)
    #> [1] 6.333333

    # Z test of the number of arcs
    zarc(A1)
    #>     z     p 
    #> 1.789 0.074

We can also check the triad census assuming conditional uniform
distribution considering different types of dyads **(U\|MAN)**

    triad_uman(A1)
    #>    label OBS   EXP   VAR   STD
    #> 1    003   0 0.083 0.076 0.276
    #> 2    012   0 0.000 0.000 0.000
    #> 3    102   2 1.750 0.688 0.829
    #> 4   021D   0 0.000 0.000 0.000
    #> 5   021U   0 0.000 0.000 0.000
    #> 6   021C   0 0.000 0.000 0.000
    #> 7   111D   0 0.000 0.000 0.000
    #> 8   111U   0 0.000 0.000 0.000
    #> 9   030T   0 0.000 0.000 0.000
    #> 10  030C   0 0.000 0.000 0.000
    #> 11   201   5 5.250 1.688 1.299
    #> 12  120D   0 0.000 0.000 0.000
    #> 13  120U   0 0.000 0.000 0.000
    #> 14  120C   0 0.000 0.000 0.000
    #> 15   210   0 0.000 0.000 0.000
    #> 16   300   3 2.917 0.410 0.640

------------------------------------------------------------------------

### Multilevel network

Now, we can calculate the degree centrality of the entire structure

    multilevel_degree(A1, B1, A2, B2, A3, B3, complete = TRUE)
    #>    multilevel bipartiteB1 bipartiteB2 bipartiteB3 tripartiteB1B2 tripartiteB1B3
    #> n1          4           1          NA           1              1              2
    #> n2          6           2          NA           1              2              3
    #> n3          3           1          NA           0              1              1
    #> n4          5           1          NA           1              1              2
    #> n5          6           2          NA           0              2              2
    #> m1          6           2           2          NA              4              2
    #> m2          6           4           1          NA              5              4
    #> m3          4           1           2          NA              3              1
    #> k1          5          NA           1           1              1              1
    #> k2          4          NA           1           2              1              2
    #> k3          4          NA           2           0              2              0
    #> k4          3          NA           1           0              1              0
    #>    tripartiteB2B3 tripartiteB1B2B3 low_multilevel meso_multilevel
    #> n1              1                2              4               2
    #> n2              1                3              6               3
    #> n3              0                1              3               1
    #> n4              1                2              5               2
    #> n5              0                2              6               2
    #> m1              2                4              4               6
    #> m2              1                5              5               6
    #> m3              2                3              3               4
    #> k1              2                2              2               2
    #> k2              3                3              3               3
    #> k3              2                2              2               2
    #> k4              1                1              1               1
    #>    high_multilevel
    #> n1               2
    #> n2               3
    #> n3               1
    #> n4               2
    #> n5               2
    #> m1               4
    #> m2               5
    #> m3               3
    #> k1               5
    #> k2               4
    #> k3               4
    #> k4               3

Besides, we can perform a *k*-core analysis of one of the levels using
the information of an incident matrix

    k_core(A1, B1, multilevel=TRUE)
    #> [1] 1 3 1 2 3

This package also allows performing complex census for multilevel
networks.

    mixed_census(A2, t(B1), B2, quad=TRUE)
    #>   000   100   001   010   020   200  11D0  11U0   120   210   220   002  01D1 
    #>     2     6     1     0     0     2     0     0     4     0     1     1     0 
    #>  01U1   012   021   022  101N  101P   201   102   202 11D1W 11U1P 11D1P 11U1W 
    #>     0     0     8     0     3     0     1     3     1     0     0     0     0 
    #>  121W  121P  21D1  21U1  11D2  11U2   221   122   212   222 
    #>    11    13     0     0     0     0     3     0     0     0
