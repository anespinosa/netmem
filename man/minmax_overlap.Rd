% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{minmax_overlap}
\alias{minmax_overlap}
\title{Minimum/maximum overlap}
\usage{
minmax_overlap(A, row = TRUE, min = TRUE)
}
\arguments{
\item{A}{A matrix object}

\item{row}{Whether to consider the actors in the rows of the matrix (default) or the column.}

\item{min}{Whether to extract the minimum (default) or the maximum overlap.}
}
\value{
This function return the overlap between the modes (a.k.a. actors, nodes, vertices).
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
minmax_overlap(A)
}
\references{
Morris, S.A. (2005). Unified Mathematical Treatment of Complex Cascaded Bipartite Networks: The Case of Collections of Journal Papers. Unpub- lished PhD Thesis, Oklahoma State University. Retrieved from \url{http://digital.library.okstate.edu/etd/umi-okstate-1334.pdf}
}
\author{
Alejandro Espinosa-Rada
}
