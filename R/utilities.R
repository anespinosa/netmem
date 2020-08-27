#' Citation networks
#'
#' Matrix transformation from incident matrices to a citation, fractional counting for co-citation or fractional counting for bibliographic coupling.
#'
#' @param A1   From incident matrix of paper and author.
#' @param A2   To incident matrix of author to paper.
#' @param citation    Character string, \dQuote{citation}, \dQuote{cocitation} and \dQuote{bcoupling}
#' 
#' @return Return a type of citation network.
#'
#' @references
#' 
#' Batagelj, V., & Cerinšek, M. (2013). On bibliographic networks. Scientometrics, 96(3), 845–864.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' 
#' A1 <- matrix(c(1,0,0,0,
#'                0,1,0,0,
#'                0,1,1,1,
#'                0,0,0,0,
#'                0,0,0,1), byrow = TRUE, ncol=4)
#' 
#' A2 <- matrix(c(1,1,1,0,0,
#'                0,0,1,0,0,
#'                0,0,1,1,0,
#'                0,0,0,1,1), byrow=TRUE, ncol=5)
#' 
#' citation_norm(A1, A2)
#' 
#' @export

citation_norm <- function(A1, A2, citation="citation"){
  Ci <- A1%*%A2
  Ci <- t(Ci)%*%Ci
  if(citation=="citation")return(Ci)

  if(citation=="cocitation"){
    D <- ifelse(rowSums(Ci)>0, rowSums(Ci), 1) 
    D <- diag(1/D)
    Cin <- t(D%*%Ci)
    coCit <- Cin%*%Ci
    return(coCit)
  }
  
  if(citation=="bcoupling"){
    D <- ifelse(rowSums(Ci)>0, rowSums(Ci), 1) 
    D <- diag(1/D)
    biCo <- Ci%*%t(Ci)
    biC <- D%*%biCo
    return(biC)
  }
}

