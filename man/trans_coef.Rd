% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/structure.R
\name{trans_coef}
\alias{trans_coef}
\title{Transitivity}
\usage{
trans_coef(
  A,
  method = c("weakcensus", "global", "mean", "local"),
  select = c("all", "in", "out")
)
}
\arguments{
\item{A}{A matrix}

\item{method}{Whether to calculate the \code{weakcensus}, \code{global} transitivity ratio, the \code{mean} transitivity or the \code{local} transitivity.}

\item{select}{Whether to consider \code{all}, \code{in} or \code{out} ties for the local transitivity.}
}
\value{
Return a transitivity measure
}
\description{
This measure is sometimes called clustering coefficient.
}
\examples{

A <- matrix(c(
  0, 1, 0, 1, 0,
  1, 0, 1, 1, 0,
  0, 1, 0, 0, 0,
  1, 1, 0, 0, 1,
  0, 0, 0, 1, 0
), byrow = TRUE, ncol = 5)
rownames(A) <- letters[1:ncol(A)]
colnames(A) <- rownames(A)

trans_coef(A, method = "local")
}
\references{
Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
}
\author{
Alejandro Espinosa-Rada
}
