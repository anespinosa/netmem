% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/similarities.R
\name{dist_sim_matrix}
\alias{dist_sim_matrix}
\title{Structural similarities}
\usage{
dist_sim_matrix(
  A,
  method = c("euclidean", "hamming", "jaccard"),
  bipartite = FALSE
)
}
\arguments{
\item{A}{A matrix}

\item{method}{The similarities/distance currently available are either \code{Euclidean} (default), \code{Hamming}, or \code{Jaccard}.}

\item{bipartite}{Whether the object is an incidence matrix}
}
\value{
This function returns a distance matrix between nodes of the same matrix.
}
\description{
In the literature of social network, Euclidean distance (Burt, 1976) or correlations (Wasserman and Faust, 1994) were considered as measures of structural equivalence.
}
\examples{
A <- matrix(c(
  0, 1, 0, 0, 1,
  0, 0, 0, 1, 1,
  0, 1, 0, 0, 1,
  0, 0, 1, 1, 0,
  0, 1, 0, 0, 0
), nrow = 5, ncol = 5, byrow = TRUE)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)
dist_sim_matrix(A, method = "jaccard")

A <- matrix(c(
  0, 0, 3, 0, 5,
  0, 0, 2, 0, 4,
  5, 4, 0, 4, 0,
  0, 3, 0, 1, 0,
  0, 0, 0, 0, 2
), nrow = 5, ncol = 5, byrow = TRUE)
dist_sim_matrix(A, method = "euclidean")
}
\references{
Burt, Ronald S. (1976) Positions in networks. Social Forces, 55(1): 93-122.

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
}
\author{
Alejandro Espinosa-Rada
}
