#' Reciprocity of Katz and Powell
#'
#' @param G   A symmetric matrix object.
#' @param fixed   Whether the choices are fixed or not
#' @param d   Numeric value of the number of fixed choices. 
#' @param dichotomic  Wheter the matrix is weighted or binary
#' 
#' @return This function gives a measurment of the tendency toward reciprocation of choices.
#'
#' @references
#'
#' Katz, L. and Powell, J.H. (1955). "Measurement of the tendency toward reciprocation of choice." Sociometry, 18:659-665.
#'
#' @author Alejandro Espinosa-Rada
#' 
#' @importFrom stats pnorm
#' 
#' @export

pkp <- function(G, fixed=FALSE, d=NULL, dichotomic=TRUE)
{
  G <- as.matrix(G)
  g <- dim(G)[1]
  if(dichotomic){
    G <- ifelse(G>=1, 1, 0)
  }
  else warning("This measure is not well specified for weighted network")
  M <- (1/2)*sum(diag(G%*%G))
  if(fixed){
    if(is.null(d)) stop("For fixed design `d` should be specified")
    (((2*(g-1))*M)-(g*(d^2)))/((g*d)*(g-1-d))
  }
  else{
    L <- sum(diag(G%*%t(G)))-sum(diag(G%*%G))
    L2 <- sum(rowSums(G)^2)
    ((2*((g-1)^2)*M)-(L^2)+L2)/((L*(g-1)^2)-(L^2)+L2)
  }
}

#' Z test for Arcs in Uniform Distribution
#'
#' @param G   A symmetric matrix object.
#' 
#' @return This function gives a Z test and p-value for arcs in uniform distribution
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge university press.
#'
#' @author Alejandro Espinosa-Rada
#' 
#' @importFrom stats pnorm
#' 
#' @export
#' 

zarc <- function(G){
  G <- as.matrix(G)
  G <- ifelse(G>0, 1, 0) # assumed dichotomic!
  l <- sum(G)
  g <- dim(G)[1]
  z <- ((l - ((g*(g-1))/2)))/(sqrt((g*(g-1))/4))
  p <- 2*pnorm(-abs(z))
  res = c(z = z,p = p)
  res
}

#' Z test for Generalized Bernoulli Models
#'
#' @param G   A symmetric matrix object.
#' @param p   p-value
#' 
#' @return This function gives a Z test and p-value for arcs in uniform distribution
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge university press.
#'
#' @author Alejandro Espinosa-Rada
#' 
#' @importFrom stats pnorm
#' 
#' @export
#' 

ZBarc <- function(G, p=0.5){
  G <- as.matrix(G)
  G <- ifelse(G>0, 1, 0) # assumed dichotomic!
  l <- sum(G)
  g <- dim(G)[1]
  q = 1-p
  z <- ((l - ((g*(g-1))*p)))/(sqrt((g*(g-1))*p*q))
  p <- 2*pnorm(-abs(z))
  res = c(z = z,p = p)
  res
}

#' Z test for Generalized Bernoulli Models
#'
#' @param G   A symmetric matrix object.
#' 
#' @return This function gives a confident interval for Bernoulli distribution
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge university press.
#'
#' @author Alejandro Espinosa-Rada
#' 
#' @importFrom stats pnorm
#'
#' @export
#' 

zIntervals <- function(G){
  G <- as.matrix(G)
  G <- ifelse(G>0, 1, 0) # assumed dichotomic!
  l <- sum(G)
  g <- dim(G)[1]
  p_maxlike <- l/(g*(g-1))
  p_lower <- p_maxlike-1.96*(sqrt((p_maxlike*(1-p_maxlike))/(g*(g-1))))
  p_upper <- p_maxlike+1.96*(sqrt((p_maxlike*(1-p_maxlike))/(g*(g-1))))
  res = c(p_maxlike = p_maxlike, p_lower = p_lower, p_upper=p_upper)
  res
}