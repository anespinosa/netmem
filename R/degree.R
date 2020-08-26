#' Generalized degree centrality for one-mode and bipartite networks.
#'
#' @param A   A symmetric matrix object
#' @param weighted    Wheter the matrix is weighted or not
#' @param type    Character string, \dQuote{out} (outdegree), \dQuote{in} (indegree) and \dQuote{all} (degree)
#' @param normalized    Wheter normalize the measure for the one-mode network (Freeman, 1978) or a bipartite network (Borgatti and Everett, 1997)
#' @param loops   Wheter if the diagonal of the matrix is considered or not
#' @param digraph   Wheter the  matrix is directed or undirected
#' @param alpha   Sets the alpha parameter in the generalised measures from Opsahl et al. (2010)
#' @param bipartite   Wheter the matrix is bipartite or not.
#' 
#' 
#' @return This function returns term 1, 2 and 3, the normalization and the maximum value of the specification of Everett and Borgatti (2020),
#' and the constraint of Burt (1992).
#'
#' @references
#'
#' Borgatti, S. P., and Everett, M. G. (1997). Network analysis of 2-mode data. Social Networks, 19(3), 243–269.
#'
#'
#' Freeman, L. C. (1978). Centrality in social networks conceptual clarification. Social Networks, 1(3), 215–239.
#'
#' Opsahl, T., Agneessens, F., and Skvoretz, J. (2010). Node centrality in weighted networks: Generalizing degree and shortest paths. Social Networks, 32(3), 245–251.
#'
#' @author Alejandro Espinosa-Rada

#' @examples
#' 
#' A <- matrix(c(0,1,0,0,0,1,
#'              1,0,1,0,0,1,
#'              1,1,0,0,0,1,
#'              1,1,0,0,1,1,
#'              0,0,0,1,0,1,
#'              1,1,1,1,1,0), ncol=6, byrow=TRUE)
#' 
#' gen_degree(A)
#' 
#' @export

gen_degree <- function(A, 
                       weighted=FALSE, type="out",
                       normalized=FALSE, loops=TRUE, 
                       digraph=TRUE,
                       alpha=0.5, bipartite=FALSE){
  A <- as.matrix(A) # matrix
  W <- A # weighted
  A[A > 0] <- 1 # binary
  n <- nrow(A) 
  
  if(!bipartite){
    if(dim(A)[1]!=dim(A)[2])stop("Adjacency matrix should be square")
    
    if(digraph){
      if(all(A[lower.tri(A)] == t(A)[lower.tri(A)]))warning("The network is undirected")
    }
    if(!digraph){
      if(type=="all")warning("For undirected networks it should be prefered type `out` that is equal to `in`")
      A[lower.tri(A)] = t(A)[lower.tri(A)] # Symmetrize
    }
    
    if(!loops){diag(A) <- 0}
    
    if(type=="in"){
      deg <- diag(t(A) %*% A) # indegree # colSums(A, na.rm=TRUE)
    }
    if(type=="out"){
      deg <- diag(A %*% t(A)) # outdegree # rowSums(A, na.rm=TRUE)  
    }
    
    if(type=="all"){
      deg <- diag(A %*% t(A))+diag(t(A) %*% A) 
    }
    
    if(weighted){
      if(type=="in"){
        si <- diag(t(W) %*% W)
      }
      if(type=="out"){
        si <- diag(W %*% t(W))
      }
      if(type=="all"){
        si <- diag(W %*% t(W))+diag(t(W) %*% W) # Freeman
      }
      deg <- (deg^(1-alpha))*(si^(alpha)) # opsahl
    }
    
    if(normalized){
      deg <- deg/(n-1)
    }
    
    if(normalized & weighted){
      stop("The normalized values should only be used for binary data")
    }
  }
  
  ### BIPARTITE
  if(bipartite){
    m <- ncol(A) # level 1
    if(dim(A)[1]==dim(A)[2])warning("Incident matrix should be rectangular")
    
    deg1 <- diag(A %*% t(A)) # level 1
    deg2 <- diag(t(A) %*% A) # level 2
    
    if(normalized){
      deg1 <- deg1/m
      deg2 <- deg2/n # BORGATII & EVERETT 1997
    }
    
    if(normalized & weighted){
      stop("The normalized values should only be used for binary data")
    }
    
    deg <- list(bipartiteL1=deg1, bipartiteL2=deg2)
  }
  return(deg)
}
