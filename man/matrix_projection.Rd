% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{matrix_projection}
\alias{matrix_projection}
\title{Unipartite projections}
\usage{
matrix_projection(A, B = NULL, digraph = FALSE)
}
\arguments{
\item{A}{A first matrix object}

\item{B}{A second matrix object}

\item{digraph}{Whether the matrix is directed or not}
}
\value{
This function return a list of matrices of the two projections of the original matrix.
}
\description{
Two-mode networks can be represented (or 'projected') as one-mode networks.
}
\examples{
A <- matrix(c(
  2, 0, 2,
  1, 1, 0,
  0, 3, 3,
  0, 2, 2,
  0, 0, 1
), byrow = TRUE, ncol = 3)
matrix_projection(A)

A <- matrix(c(
  0, 0, 0, 0, 1,
  1, 0, 0, 0, 0,
  1, 1, 0, 0, 0,
  0, 1, 1, 1, 1,
  0, 0, 1, 0, 0,
  0, 0, 1, 1, 0
), byrow = TRUE, ncol = 5)

B <- matrix(c(
  0, 0, 0, 0, 1,
  1, 0, 0, 0, 0,
  1, 0, 0, 0, 0,
  0, 1, 0, 0, 0,
  0, 0, 1, 0, 0,
  0, 0, 1, 0, 0
), byrow = TRUE, ncol = 5)
matrix_projection(A, B, digraph = TRUE)
}
\references{
Davis, Allison; Gardner, Burleigh B. and Mary. R. Gardner (1941). Deep South: A Social Anthropological Study of Caste and Class. The University of Chicago Press, Chicago.

Breiger, Ronald L. (1976). The Duality of Persons and Groups, 53(2), 181-190 doi: \url{https://doi.org/10.2307/2576011}

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
}
\author{
Alejandro Espinosa-Rada
}
