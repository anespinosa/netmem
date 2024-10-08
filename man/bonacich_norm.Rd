% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/similarities.R
\name{bonacich_norm}
\alias{bonacich_norm}
\title{Bonacich normalization}
\source{
Adapted from Borgatti, S., Everett, M., Johnson, J. and Agneessens, P. (2022) Analyzing Social Networks Using R. Sage.
}
\usage{
bonacich_norm(A, projection = c("rows", "columns"), normalisation = FALSE)
}
\arguments{
\item{A}{An incidence matrix}

\item{projection}{Whether to normalise by \code{rows} (default), or \code{columns} of the matrix.}

\item{normalisation}{Normalise the measure}
}
\value{
This function returns the Bonacich normalisation.
}
\description{
The function provide a normalisation provided by Bonacich (1972).
}
\examples{
A <- matrix(
  c(
    1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0,
    1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
    0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
    1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1,
    0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0
  ),
  byrow = TRUE, ncol = 14
)
bonacich_norm(A)
}
\references{
Bonacich, P. (1972). Factoring and weighting approaches to status scores and clique identification. Journal of Mathematical Sociology, 2: 112-120.
}
