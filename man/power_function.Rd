% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{power_function}
\alias{power_function}
\title{Power matrix}
\usage{
power_function(A, n)
}
\arguments{
\item{A}{A matrix}

\item{n}{Positive integer}
}
\value{
This function return the power of a matrix by repeating matrix multiplication.
}
\description{
Power of a matrix computed by successive matrix multiplication.
}
\examples{
A <- matrix(c(
  1, 0, 0, 0,
  1, 1, 0, 0,
  1, 0, 1, 0,
  0, 1, 1, 1
), byrow = TRUE, ncol = 4, nrow = 4)
power_function(A, 1000)

}
\references{
Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
}
\author{
Alejandro Espinosa-Rada
}
