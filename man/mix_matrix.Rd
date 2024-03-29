% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/homophily.R
\name{mix_matrix}
\alias{mix_matrix}
\title{Mixing matrix}
\usage{
mix_matrix(A, att = NULL)
}
\arguments{
\item{A}{A symmetric matrix object}

\item{att}{Categorical attribute of the nodes}
}
\value{
This function returns a mixing matrix.
}
\description{
Create a mixing matrix from node attributes. The mixing matrix is a two-dimensional
matrix that cross-classifies the edges depending on the values of their attributes.
This matrix allowed identifying segregation and homophily at the network level.
}
\details{
Values in the diagonal are the number of ties within groups, and off-diagonal are the number of relations between groups.
}
\examples{

n <- 100
A <- matrix(c(rbinom(n, 1, 0.5)),
  ncol = sqrt(n), nrow = sqrt(n), byrow = TRUE
)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- letters[1:ncol(A)]
att <- rbinom(sqrt(n), 3, 0.5)
mix_matrix(A, att = att)
}
\author{
Alejandro Espinosa-Rada
}
