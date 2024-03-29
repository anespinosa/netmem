% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/structure.R
\name{trans_matrix}
\alias{trans_matrix}
\title{Transitivity matrix}
\usage{
trans_matrix(A, loops = FALSE)
}
\arguments{
\item{A}{A matrix}

\item{loops}{Whether to expect nonzero elements in the diagonal of the matrix}
}
\value{
A vector assigning an id the components that each of the nodes of the matrix belongs
}
\description{
This function assigns a one in the elements of the matrix if a group of actors are part of a transitivity structure (030T label considering the MAN triad census)
}
\examples{

A <- matrix(
  c(
    0, 1, 1, 0, 0, 0,
    0, 0, 1, 0, 0, 0,
    0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0,
    0, 0, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0
  ),
  byrow = TRUE, ncol = 6
)
rownames(A) <- letters[1:NROW(A)]
colnames(A) <- rownames(A)
trans_matrix(A, loops = TRUE)

}
\references{
Davis, J.A. and Leinhardt, S. (1972). “The Structure of Positive Interpersonal Relations in Small Groups.” In J. Berger (Ed.), Sociological Theories in Progress, Vol. 2, 218-251. Boston: Houghton Mifflin.

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
}
\author{
Alejandro Espinosa-Rada
}
