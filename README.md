
<!-- README.md is generated from README.Rmd. Please edit that file -->

netmem: Social Network Measures using Matrices
==============================================

<!-- badges: start -->
<!-- badges: end -->

The goal of netmem is to make available different measures to describe
social networks using matrices.

-   Author/mantainer: [Alejandro
    Espinosa-Rada](https://www.research.manchester.ac.uk/portal/en/researchers/alejandro-espinosa(4ed72800-e02b-47a8-a958-640b6a07f563).html)

Installation
------------

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("anespinosa/netmem")

Example
-------

    library(netmem)

Let’s assume that we have a multilevel network with three adjacency
matrices and two incident matrices between them.

-   `A1`: Adjacency Matrix of the level 1
-   `B1`: Incident Matrix between level 1 and level 2
-   `A2`: Adjacency Matrix of the level 2
-   `B2`: Incident Matrix between level 2 and level 3
-   `A3`: Adjacency Matrix of the leve 3

We could create the data using matrices

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

    A3 <- matrix(c(0,1,1,1,
                  1,0,0,0,
                  1,0,0,1,
                  1,0,1,0), byrow=TRUE, ncol=4)

    B3 <- matrix(c(1,0,0,0,0,
                  0,1,0,1,0,
                  0,0,0,0,0,
                  0,0,0,0,0), byrow=TRUE, ncol=5)

We could first explore a bit the data

    # Degree centrality of the first matrix
    gen_degree(A1, digraph = FALSE)

    # Degree centrality of one of bipartite network
    gen_degree(B1, bipartite = TRUE)

We can also calculate the level of constraint of one particular actor
(e.g. actor `5`)

    eb_constraint(A1, ego=5)

Now, we can calculate the degree centrality of the entire stucture

    multilevel_degree(A1, B1, A2, B2, A3, B3, complete = TRUE)
