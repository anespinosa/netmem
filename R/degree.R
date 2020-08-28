#' Generalized degree centrality for one-mode and bipartite networks.
#'
#' @param A   A symmetric matrix object
#' @param weighted    Wheter the matrix is weighted or not
#' @param type    Character string, \dQuote{out} (outdegree), \dQuote{in} (indegree) and \dQuote{all} (degree)
#' @param normalized    Wheter normalize the measure for the one-mode network (Freeman, 1978) or a bipartite network (Borgatti and Everett, 1997)
#' @param loops   Wheter the diagonal of the matrix is considered or not
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
  A <- as.matrix(A)
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


#' Generalized K-core 
#' 
#' Generalized k-core for undirected, directed, weighted and multilevel networks
#'
#' @param A   A symmetric matrix object.
#' @param B1  An incident matrix for multilevel networks.
#' @param multilevel   Wheter the measure of k-core is for multilevel networks.
#' @param weighted    Wheter the measure of k-core is for valued matrices
#' @param type    Character string, \dQuote{out} (outdegree), \dQuote{in} (indegree) and \dQuote{all} (degree)
#' @param loops   Wheter the diagonal of the matrix is considered or not
#' @param digraph   Wheter the  matrix is directed or undirected
#' @param alpha   Sets the alpha parameter in the generalised measures from Opsahl et al. (2010)
#' 
#' @return This function return the k-core.
#'
#' @references
#'
#' Batagelj, V., & Zaveršnik, M. (2011). Fast algorithms for determining (generalized) core groups in social networks. Advances in Data Analysis and Classification, 5(2), 129–145. \url{https://doi.org/10.1007/s11634-010-0079-y}
#'
#' Eidsaa, M., & Almaas, E. (2013). s-core network decomposition: A generalization of $k$-core analysis to weighted networks. Physical Review E, 88(6), 062819. \url{https://doi.org/10.1103/PhysRevE.88.062819}
#' 
#' Espinosa-Rada, A. (forthcoming). A Network Approach for the Sociological Study of Science: Modelling Dynamic Multilevel Networks. [PhD]. The University of Manchester.
#'
#' Seidman S (1983).  'Network structure and minimum degree'.  Social Networks, 5, 269-287.
#' 
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' 
#' A1 <- matrix(c(0,1,0,0,0,
#'                1,0,0,1,0,
#'                0,0,0,1,0,
#'                0,1,1,0,1,
#'                0,0,0,1,0), byrow=TRUE, ncol=5)
#' B1 <- matrix(c(1,0,0,
#'                1,1,0,
#'                0,1,0,
#'                0,1,0,
#'                0,1,1), byrow=TRUE, ncol=3)
#' 
#' k_core(A1, B1, multilevel=TRUE)
#' 
#' @export


k_core <- function (A, B1=NULL, 
                    multilevel=FALSE, type="in", 
                    digraph=FALSE, loops=FALSE,
                    weighted=FALSE, alpha=1) 
{
  if(!weighted & !multilevel){
    if(digraph){g <- igraph::graph.adjacency(A, mode=c("directed"))}
    if(!digraph){g <- igraph::graph.adjacency(A, mode=c("undirected"))}
    return(coreness(g, mode=c(type)))
  }
  if(!multilevel & weighted){
    if(!is.null(B1))stop("Matrix `B1` for multilevel networks")
    W <- A
    ct <- 1
    k.core <- vector("integer", length = nrow(W))
    repeat {
      str.tmp <- gen_degree(W, digraph=digraph, type=type,
                            alpha=alpha,
                            loops=loops, weighted=weighted) 
      s.thr <- min(str.tmp[which(str.tmp > 0)])
      v.remove <- which(str.tmp <= s.thr & str.tmp > 0)
      if (length(v.remove) > 0) {
        k.core[v.remove] <- ct
        W[v.remove, ] <- W[, v.remove] <- 0
        ct <- ct + 1
      }
      if (sum(colSums(W) > 0) == 0) 
        break
    }
  }
  if(multilevel){
    if(is.null(B1))stop("A bipartite network should be added for multilevel networks")
    W <- A
    ct <- 1
    k.core <- vector("integer", length = nrow(W))
    repeat {
      str.tmp <- multilevel_degree(W, B1, 
                                   weightedA1 = weighted,
                                   typeA1 = type, alphaA1 = alpha,
                                   loopsA1 = loops)
      str.tmp <- str.tmp$multilevel
      str.tmp <- head(str.tmp, n=dim(W)[1]) 
      s.thr <- min(str.tmp[which(str.tmp > 0)])
      v.remove <- which(str.tmp <= s.thr & str.tmp > 0)
      if (length(v.remove) > 0) {
        k.core[v.remove] <- ct
        W[v.remove, ] <- W[, v.remove] <- 0
        B1[v.remove, ] <- 0 # CHECK
        
        ct <- ct + 1
      }
      if (sum(colSums(W) > 0) == 0) 
        break
    }
  }
  return(k.core)
}

