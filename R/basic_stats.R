#' Reciprocity of Katz and Powell
#'
#' @param G   A symmetric matrix object.
#' @param fixed   Whether the choices are fixed or not
#' @param d   Numeric value of the number of fixed choices.
#' @param dichotomic  Whether the matrix is weighted or binary
#'
#' @return This function gives a measurment of the tendency toward 
#' reciprocation of choices.
#'
#' @references
#'
#' Katz, L. and Powell, J.H. (1955). "Measurement of the tendency toward 
#' reciprocation of choice." Sociometry, 18:659-665.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' data(krackhardt_friends)
#' kp_reciprocity(krackhardt_friends, fixed = TRUE, d = 5)
#' @importFrom stats pnorm
#'
#' @export

kp_reciprocity <- function(G, fixed = FALSE, d = NULL, dichotomic = TRUE) {
  G <- as.matrix(G)
  g <- dim(G)[1]
  if (dichotomic) {
    G <- ifelse(G >= 1, 1, 0)
  } else {
    warning("This measure is not well specified for weighted network")
  }
  M <- (1 / 2) * sum(diag(G %*% G))
  if (fixed) {
    if (is.null(d)) stop("For fixed design `d` should be specified")
    (((2 * (g - 1)) * M) - (g * (d^2))) / ((g * d) * (g - 1 - d))
  } else {
    L <- sum(diag(G %*% t(G))) - sum(diag(G %*% G))
    L2 <- sum(rowSums(G)^2)
    ((2 * ((g - 1)^2) * M) - (L^2) + L2) / ((L * (g - 1)^2) - (L^2) + L2)
  }
}

#' Z test of the number of arcs
#'
#' @param G   A symmetric matrix object.
#' @param p   Constant probability p.
#' @param interval    Return a 95 percent confidence interval.
#'
#' @return This function gives a Z test and p-value for the number of lines or arcs present in a directed graph
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' data(krackhardt_friends)
#' z_arctest(krackhardt_friends)
#' @importFrom stats pnorm
#'
#' @export
#'

z_arctest <- function(G, p = 0.5, interval = FALSE) {
  G <- as.matrix(G)
  G <- ifelse(G > 0, 1, 0)
  l <- sum(G)
  g <- dim(G)[1]
  q <- 1 - p
  z <- ((l - ((g * (g - 1)) * p))) / (sqrt((g * (g - 1)) * p * q))
  p <- 2 * pnorm(-abs(z))
  res <- round(c(z = z, p = p), 3)

  if (interval) {
    p_maxlike <- l / (g * (g - 1))
    p_lower <- p_maxlike - 1.96 * (sqrt((p_maxlike * (1 - p_maxlike)) / (g * (g - 1))))
    p_upper <- p_maxlike + 1.96 * (sqrt((p_maxlike * (1 - p_maxlike)) / (g * (g - 1))))
    res <- round(c(
      z = z, p = p,
      p_maxlike = p_maxlike, p_lower = p_lower, p_upper = p_upper
    ), 3)
    res
  }

  res
}
