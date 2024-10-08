% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/census.R
\name{multiplex_census}
\alias{multiplex_census}
\title{Multiplex triad census}
\usage{
multiplex_census(A, B)
}
\arguments{
\item{A}{A directed matrix object.}

\item{B}{An undirected matrix object.}
}
\value{
This function gives the counts of the mixed multiplex triad census for a directed and an undirected network.
}
\description{
This function counts the different subgraphs of three nodes in a multiplex directed and undirected network.
}
\examples{

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
}
\references{
Espinosa-Rada, A. (2021). A Network Approach for the Sociological Study of Science: Modelling Dynamic Multilevel Networks. [PhD](https://research.manchester.ac.uk/en/studentTheses/a-network-approach-for-the-sociological-study-of-science-and-know). The University of Manchester.

Espinosa-Rada, A., Bellotti, E., Everett, M., & Stadtfeld, C. (2024). Co-evolution of a socio-cognitive scientific network: A case study of citation dynamics among astronomers. Social Networks, 78, 92–108. https://doi.org/10.1016/j.socnet.2023.11.008
}
\author{
Alejandro Espinosa-Rada
}
