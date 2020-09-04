#' Generalized degree
#' 
#' Generalized degree centrality for one-mode and bipartite networks
#'
#' @param A   A matrix object
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
#' and the constraint of Burt (1992)
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
#' A3 <- matrix(c(0,4,4,0,0,0,
#'                4,0,2,1,1,0,
#'                4,2,0,0,0,0,
#'                0,1,0,0,0,0,
#'                0,1,0,0,0,7,
#'                0,0,0,0,7,0), byrow=TRUE, ncol=6)
#' 
#' gen_degree(A3, digraph = FALSE, weighted=TRUE)
#' 
#' @export

gen_degree <- function(A, 
                       weighted=FALSE, type="out",
                       normalized=FALSE, loops=TRUE, 
                       digraph=TRUE,
                       alpha=0.5, bipartite=FALSE){
  A <- as.matrix(A)
  W <- A
  A[A > 0] <- 1
  n <- nrow(A) 
  
  if(!bipartite){
    if(dim(A)[1]!=dim(A)[2])stop("Adjacency matrix should be square")
    
    if(digraph){
      if(all(A[lower.tri(A)] == t(A)[lower.tri(A)]))warning("The network is undirected")
    }
    if(!digraph){
      if(type=="all")warning("For undirected networks it should be prefered type `out` that is equal to `in`")
      A[lower.tri(A)] = t(A)[lower.tri(A)]
    }
    
    if(!loops){diag(A) <- 0}
    
    if(type=="in"){
      deg <- colSums(A, na.rm=TRUE)
    }
    if(type=="out"){
      deg <- rowSums(A, na.rm=TRUE)
    }
    
    if(type=="all"){
      deg <- colSums(A, na.rm=TRUE)+rowSums(A, na.rm=TRUE)
    }
    
    if(weighted){
      if(type=="in"){
        si <- colSums(W, na.rm=TRUE)
      }
      if(type=="out"){
        si <- rowSums(W, na.rm=TRUE)
      }
      if(type=="all"){
        si <- colSums(W, na.rm=TRUE)+rowSums(W, na.rm=TRUE)
      }
      deg <- (deg^(1-alpha))*(si^(alpha)) 
    }
    
    if(normalized){
      deg <- deg/(n-1)
    }
    
    if(normalized & weighted){
      stop("The normalized values should only be used for binary data")
    }
  }
  
  if(bipartite){
    m <- ncol(A) 
    if(dim(A)[1]==dim(A)[2])warning("Incident matrix should be rectangular")
    
    deg1 <- diag(A %*% t(A))
    deg2 <- diag(t(A) %*% A)
    
    if(normalized){
      deg1 <- deg1/m
      deg2 <- deg2/n
    }
    
    if(normalized & weighted){
      stop("The normalized values should only be used for binary data")
    }
    
    deg <- list(bipartiteL1=deg1, bipartiteL2=deg2)
  }
  return(deg)
}


#' Degree centrality for multilevel networks.
#'
#' @param A1  The square matrix of the lowest level
#' @param B1  The incident matrix of the ties between the nodes of first level and the nodes of the second level
#' @param A2  The square matrix of the second level
#' @param B2  The incident matrix of the ties between the nodes of the second level and the nodes of the third level
#' @param A3  The square matrix of the third level
#' @param B3  The incident matrix of the ties between the nodes of the third level and the nodes of the first level
#' @param complete  Add the degree of bipartite and tripartite networks for B1, B2 and/or B3, and the low_multilevel (i.e. A1+B1+B2+B3), meso_multilevel (i.e. B1+A2+B2+B3) and high_multilevel (i.e. B1+B2+A3+B3) degree
#' @param digraphA1  Wheter A1 is a directed network
#' @param digraphA2  Wheter A2 is a directed network
#' @param digraphA3  Wheter A3 is a directed network
#' @param typeA1  Type of degree of the network for A1, "out" for out-degree, "in" for in-degree or "all" for the sum of the two
#' @param typeA2  Type of degree of the network for A2, "out" for out-degree, "in" for in-degree or "all" for the sum of the two
#' @param typeA3  Type of degree of the network for A3, "out" for out-degree, "in" for in-degree or "all" for the sum of the two
#' @param loopsA1  Wheter the loops of the edges are considered in matrix A1
#' @param loopsA2  Wheter the loops of the edges are considered in matrix A2
#' @param loopsA3  Wheter the loops of the edges are considered in matrix A3
#' @param normalized If TRUE then the result is divided by (n-1)+k+m for the first level, (m-1)+n+k for the second level, and (k-1)+m+n according to Espinosa-Rada et al. (2021)
#' @param weightedA1  Wheter A1 is weighted
#' @param weightedA2  Wheter A2 is weighted
#' @param weightedA3  Wheter A3 is weighted
#' @param alphaA1  The alpha parameter of A1 according to Opsahl et al (2010) for weighted networks. The value 0.5 is given by default.
#' @param alphaA2  The alpha parameter of A2 according to Opsahl et al (2010) for weighted networks. The value 0.5 is given by default.
#' @param alphaA3  The alpha parameter of A3 according to Opsahl et al (2010) for weighted networks. The value 0.5 is given by default.
#' 
#' @return Return a data.frame of multilevel degree
#'
#' @references
#'
#' Borgatti, S. P., and Everett, M. G. (1997). Network analysis of 2-mode data. Social Networks, 19(3), 243–269.
#'
#' Espinosa-Rada, Alejandro; Bellotti, Elisa; Everett, Martin & Stadtfeld, Christoph (forthcoming). "Co-evolution of Scientific Networks: Multilevel Analysis of a National Discipline"
#' 
#' Freeman, L. C. (1978). Centrality in social networks conceptual clarification. Social Networks, 1(3), 215–239.
#'
#' Opsahl, T., Agneessens, F., and Skvoretz, J. (2010). Node centrality in weighted networks: Generalizing degree and shortest paths. Social Networks, 32(3), 245–251.
#'
#' @import utils
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' 
#'A1 <- matrix(c(0,1,0,0,0,
#'               1,0,0,1,0,
#'               0,0,0,1,0,
#'               0,1,1,0,1,
#'               0,0,0,1,0), byrow=TRUE, ncol=5)
#'               
#'B1 <- matrix(c(1,0,0,
#'               1,1,0,
#'               0,1,0,
#'               0,1,0,
#'               0,1,1), byrow=TRUE, ncol=3)
#'
#'A2 <- matrix(c(0,1,1,
#'               1,0,0,
#'               1,0,0), byrow=TRUE, nrow=3)
#'
#'B2 <- matrix(c(1,1,0,0,
#'               0,0,1,0,
#'               0,0,1,1), byrow=TRUE, ncol=4)
#'
#'A3 <- matrix(c(0,1,1,1,
#'               1,0,0,0,
#'               1,0,0,1,
#'               1,0,1,0), byrow=TRUE, ncol=4)
#'
#'B3 <- matrix(c(1,0,0,0,0,
#'               0,1,0,1,0,
#'               0,0,0,0,0,
#'               0,0,0,0,0), byrow=TRUE, ncol=5)
#' 
#' multilevel_degree(A1, B1, A2, B2, A3, B3)
#' 
#' \dontrun{
#' multilevel_degree(A1, B1, A2, B2, A3, B3, normalized=TRUE, complete=TRUE)
#' }
#' 
#' @export
#' 
#' 

multilevel_degree <- function(A1, B1, 
                              A2=NULL, B2=NULL, 
                              A3=NULL, B3=NULL, 
                              complete=FALSE,
                              digraphA1=FALSE,
                              digraphA2=FALSE,
                              digraphA3=FALSE,
                              typeA1="out",
                              typeA2="out",
                              typeA3="out",
                              loopsA1=FALSE,
                              loopsA2=FALSE,
                              loopsA3=FALSE,
                              normalized=FALSE,
                              weightedA1=FALSE,
                              weightedA2=FALSE,
                              weightedA3=FALSE,
                              alphaA1=0.5,
                              alphaA2=0.5,
                              alphaA3=0.5){
  A1 <- as.matrix(A1)
  if(!weightedA1){
    if(!all(A1 %in% 0:1))warning("Matrix `A1`  is weighted and would be treated as binary")
    A1[A1 > 0] <- 1 
  }
  n <- nrow(A1) 
  
  if(is.null(A2)){
    A2 <- matrix(0, byrow=TRUE, ncol=dim(B1)[2], nrow=dim(B1)[2])}
  
  if(is.null(A3)){
    if(!is.null(B2)){A3 <- matrix(0, byrow=TRUE, 
                                  ncol=dim(B2)[2], nrow=dim(B2)[2]) }}
  
  if(weightedA1 & normalized){stop("The normalized values should only be used for binary data")}
  if(weightedA2 & normalized){stop("The normalized values should only be used for binary data")}
  if(weightedA3 & normalized){stop("The normalized values should only be used for binary data")}
  
  if(!is.null(A3)){
    if(is.null(B1))stop("Is not available bipartite network between levels")
  }
  if(!is.null(B1)){
    m <- ncol(B1) 
    if((!all(B1 %in% 0:1) & normalized))stop("Matrix `B1` is weighted and the normalized values should only be used for binary data")
    if(!all(B1 %in% 0:1))warning("Matrix `B1` is weighted")
    if(!dim(A1)[1]==dim(B1)[1])stop("Non-conformable arrays")
    if(dim(B1)[1]==dim(B1)[2])warning("Matrix should be rectangular")
    deg1 <- diag(B1 %*% t(B1)) 
    deg2 <- diag(t(B1) %*% B1)
    
    M1 <- cbind(A1,B1) 
    M1b <- cbind(t(B1),
                 matrix(0, nrow=(dim(M1)[2]-dim(B1)[1]), byrow=T, 
                        ncol=(dim(M1)[2]-dim(A1)[1]))
    )
    M1 <- rbind(M1, M1b)
    degL1 <- gen_degree(M1 ,type=typeA1, loops=loopsA1, digraph=digraphA1,
                        weighted=weightedA1, alpha=alphaA1)
    degL1 <- head(degL1, n=n) 
    
    if(normalized){
      deg1 <- deg1/m
      deg2 <- deg2/n 
      degL1 <- degL1/((n-1)+m)
    }
  }
  
  if(!is.null(A2)){
    if(is.null(B1))stop("Multilevel networks require at least one bipartite network between levels")
    if(!dim(A2)[1]==dim(A2)[2])stop("Matrix should be square")
    
    if(!weightedA2){
      if(!all(A2 %in% 0:1))warning("Matrix `A2` is weighted and would be treated as binary")
      A2[A2 > 0] <- 1 
    }
    if(!dim(B1)[2]==dim(A2)[1])stop("Non-conformable arrays")
    M2 <- cbind(A2,t(B1))
    M2b <- cbind(B1,
                 matrix(0, nrow=(dim(M2)[2]-dim(B1)[2]), byrow=T, 
                        ncol=(dim(A1)[1])))
    M2 <- rbind(M2, M2b)
    degL2 <- gen_degree(M2 ,type=typeA2, loops=loopsA2, digraph=digraphA2,
                        weighted=weightedA2, alpha=alphaA2)
    degL2 <- head(degL2, n=m)
    names <- c(paste(rep("n", dim(A1)[1]), 1:dim(A1)[1], sep=""),
               paste(rep("m", dim(B1)[2]), 1:dim(B1)[2], sep=""))
    multilevel <- c(degL1, degL2)
    bipartite <- c(deg1, deg2)
    deg <- as.data.frame(cbind(multilevel, bipartite))
    rownames(deg) <- names
    
    if(normalized){
      degL2 <- degL2/((m-1)+n)
      names <- c(paste(rep("n", dim(A1)[1]), 1:dim(A1)[1], sep=""),
                 paste(rep("m", dim(B1)[2]), 1:dim(B1)[2], sep=""))
      multilevel <- c(degL1, degL2)
      bipartite <- c(deg1, deg2)
      deg <- as.data.frame(cbind(multilevel, bipartite))
      rownames(deg) <- names
      
    }
    if(!complete){
      names <- c(paste(rep("n", dim(A1)[1]), 1:dim(A1)[1], sep=""),
                 paste(rep("m", dim(B1)[2]), 1:dim(B1)[2], sep=""))
      multilevel <- c(degL1, degL2)
      deg <- as.data.frame(multilevel)
      rownames(deg) <- names
      
    }
  }
  else{A2 <- matrix(0, byrow=TRUE, ncol=dim(B1)[2], nrow=dim(B1)[2])}
  
  if(!is.null(B2)){
    if((!all(B2 %in% 0:1) & normalized))stop("Matrix `B2` is weighted and the normalized values should only be used for binary data")
    if(!all(B2 %in% 0:1))warning("Matrix `B2` is weighted")
    if(is.null(B1))stop("Is not available the first bipartite network between lower and medium levels")
    if(dim(B2)[1]==dim(B2)[2])warning("Matrix should be rectangular")
    if(!dim(A2)[1]==dim(B2)[1])stop("Non-conformable arrays")
    k <- ncol(B2)
    deg3 <- diag(B2 %*% t(B2)) 
    deg4 <- diag(t(B2) %*% B2) 
    M3 <- cbind(A2,B2)
    M3b <- cbind(t(B2),
                 matrix(0, nrow=(dim(B2)[2]), byrow=T, 
                        ncol=(dim(B2)[2])))
    M3 <- rbind(M3, M3b)
    degL3 <- gen_degree(M3, type=typeA2, loops=loopsA2, digraph=digraphA2,
                        weighted=weightedA2, alpha=alphaA2)
    degL3 <- head(degL3, n=k)
    M2M3a <- cbind(A2,t(B1), B2)
    M2M3b <- cbind(B1, matrix(0, nrow=(dim(B1)[1]), byrow=T, 
                              ncol=(dim(B1)[1])), 
                   matrix(0, nrow=(dim(B1)[1]), byrow=T, 
                          ncol=(dim(B2)[2])))
    M2M3c <- cbind(t(B2), matrix(0, nrow=(dim(B2)[2]), byrow=T, 
                                 ncol=(dim(B1)[1])), 
                   matrix(0, nrow=(dim(B2)[2]), byrow=T,
                          ncol=(dim(B2)[2])))
    M2M3 <- rbind(M2M3a, M2M3b, M2M3c)
    L1B1L2B2 <- gen_degree(M2M3, type=typeA2, loops=loopsA2, digraph=digraphA2,
                           weighted=weightedA2, alpha=alphaA2)
    L1B1L2B2 <- head(L1B1L2B2, n=m)
    
    if(normalized){
      deg3 <- deg3/k
      deg4 <- deg4/m 
      degL3=degL3/((m-1)+k)
      L1B1L2B2 <- L1B1L2B2/(n+(m-1)+k)
    }
    names <- c(paste(rep("n", dim(A1)[1]), 1:dim(A1)[1], sep=""),
               paste(rep("m", dim(B1)[2]), 1:dim(B1)[2], sep=""),
               paste(rep("k", dim(B2)[2]), 1:dim(B2)[2], sep=""))
    multilevel <- c(degL1, L1B1L2B2, degL3)
    bipartiteB1 <- c(deg1, deg2, rep(NA, dim(B2)[2]))
    bipartiteB2 <- c(rep(NA, dim(A1)[1]), deg3, deg4)
    tripartiteB1B2 <- c(deg1, (deg2+deg3), deg4)
    deg <- as.data.frame(cbind(multilevel, bipartiteB1,
                               bipartiteB2, tripartiteB1B2))
    rownames(deg) <- names
    
    
    if(!complete){
      names <- c(paste(rep("n", dim(A1)[1]), 1:dim(A1)[1], sep=""),
                 paste(rep("m", dim(B1)[2]), 1:dim(B1)[2], sep=""),
                 paste(rep("k", dim(B2)[2]), 1:dim(B2)[2], sep=""))
      multilevel <- c(degL1, L1B1L2B2, degL3)
      deg <- as.data.frame(multilevel)
      rownames(deg) <- names
    }
    
    
    if(!is.null(A3)){
      if(!dim(A3)[1]==dim(A3)[2])stop("Matrix should be square")
      if(!weightedA3){
        if(!all(A3 %in% 0:1))warning("Matrix `A3` is weighted and would be treated as binary")
        A3[A3 > 0] <- 1 
      }
      if(!dim(B2)[2]==dim(A3)[1])stop("Non-conformable arrays")
      M4 <- cbind(A3,t(B2))
      M4b <- cbind(B2,
                   matrix(0, nrow=(dim(B1)[2]), byrow=T, 
                          ncol=(dim(B1)[2])))
      M4 <- rbind(M4, M4b)
      degL4 <- gen_degree(M4, type=typeA3, loops=loopsA3, digraph=digraphA3,
                          weighted=weightedA3, alpha=alphaA3)
      degL4 <- head(degL4, n=k)
      
      if(normalized){
        degL4 <- degL4/((k-1)+m)
      }
      
      if(!is.null(A2)){
        names <- c(paste(rep("n", dim(A1)[1]), 1:dim(A1)[1], sep=""),
                   paste(rep("m", dim(B1)[2]), 1:dim(B1)[2], sep=""),
                   paste(rep("k", dim(B2)[2]), 1:dim(B2)[2], sep=""))
        multilevel <- c(degL1, L1B1L2B2, degL3)
        bipartiteB1 <- c(deg1, deg2, rep(NA, dim(B2)[2]))
        bipartiteB2 <- c(rep(NA, dim(A1)[1]), deg3, deg4)
        tripartiteB1B2 <- c(deg1, (deg2+deg3), deg4)
        
        low_multilevel <- c(degL1, (deg2+deg3), deg4)
        meso_multilevel <- c(deg1, L1B1L2B2, deg4)
        high_multilevel <- c(deg1,  (deg2+deg3), degL4)
        
        deg <-as.data.frame(cbind(multilevel, bipartiteB1,
                                  bipartiteB2, tripartiteB1B2,
                                  low_multilevel, 
                                  meso_multilevel,
                                  high_multilevel))
        rownames(deg) <- names
        
      }
      
      if(!complete){
        names <- c(paste(rep("n", dim(A1)[1]), 1:dim(A1)[1], sep=""),
                   paste(rep("m", dim(B1)[2]), 1:dim(B1)[2], sep=""),
                   paste(rep("k", dim(B2)[2]), 1:dim(B2)[2], sep=""))
        multilevel <- c(degL1, L1B1L2B2, degL3)
        deg <- as.data.frame(multilevel)
        rownames(deg) <- names
      }
    }
    else{A3 <- matrix(0, byrow=TRUE, ncol=dim(B2)[2], nrow=dim(B2)[2])}
    
    if(!is.null(B3)){
      if(is.null(B2)){B2 <- matrix(0, byrow=TRUE, ncol=dim(B1)[2], nrow=dim(B3)[1])}
      if((!all(B3 %in% 0:1) & normalized))stop("Matrix `B3` is weighted and the normalized values should only be used for binary data")
      if(!all(B3 %in% 0:1))warning("Matrix `B3` is weighted")
      if(!dim(B3)[1]==dim(A3)[2])stop("Non-conformable arrays")
      if(!dim(B3)[2]==dim(A1)[1])stop("Non-conformable arrays")
      
      deg5 <- diag(B3 %*% t(B3))
      deg6 <- diag(t(B3) %*% B3)
      CM1a <- cbind(A1, B1, t(B3))
      CM1b <- cbind(t(B1), A2, matrix(0, nrow=(dim(B1)[2]), byrow=T, 
                                      ncol=(dim(B3)[1])))
      CM1c <- cbind(B3, matrix(0, nrow=(dim(B3)[1]), byrow=T, 
                               ncol=(dim(B1)[2])), A3)
      CM1 <- rbind(CM1a, CM1b, CM1c)
      CM1 <- gen_degree(CM1, type=typeA1, loops=loopsA1, digraph=digraphA1,
                        weighted=weightedA1, alpha=alphaA1)
      CM1 <- head(CM1, n=n)
      CM3a <- cbind(A3, t(B2), B3)
      CM3b <- cbind(B2,matrix(0, nrow=(dim(B1)[2]), byrow=T, 
                              ncol=(dim(B1)[2])), 
                    matrix(0, nrow=(dim(B1)[2]), byrow=T,
                           ncol=(dim(B3)[2])))
      CM3c <- cbind(t(B3), matrix(0, nrow=(dim(B3)[2]), byrow=T, 
                                  ncol=(dim(B1)[2])), 
                    matrix(0, nrow=(dim(B3)[2]), byrow=T, 
                           ncol=(dim(B3)[2])))
      CM3 <- rbind(CM3a, CM3b, CM3c)
      CM3 <- gen_degree(CM3, type=typeA3, loops=loopsA3, digraph=digraphA3,
                        weighted=weightedA3, alpha=alphaA3)
      CM3 <- head(CM3, n=k)
      
      if(!is.null(A2)){
        names <- c(paste(rep("n", dim(A1)[1]), 1:dim(A1)[1], sep=""),
                   paste(rep("m", dim(B1)[2]), 1:dim(B1)[2], sep=""),
                   paste(rep("k", dim(B3)[1]), 1:dim(B3)[1], sep=""))
        multilevel <- c(CM1, L1B1L2B2, CM3)
        bipartiteB1 <- c(deg1, deg2, rep(NA, dim(B2)[2]))
        bipartiteB2 <- c(rep(NA, dim(A1)[1]), deg3, deg4)
        bipartiteB3 <- c(deg6, rep(NA, m), deg5)
        tripartiteB1B2 <- c(deg1, (deg2+deg3), deg4)
        tripartiteB1B3 <- c((deg1+deg6), deg2, deg5)
        tripartiteB2B3 <- c(deg6, deg3, (deg4+deg5))
        tripartiteB1B2B3 <- c((deg1+deg6), (deg2+deg3), (deg4+deg5))
        low_multilevel <- c(CM1, (deg2+deg3), (deg4+deg5))
        meso_multilevel <- c((deg1+deg6), L1B1L2B2, (deg4+deg5))
        high_multilevel <- c((deg1+deg6),  (deg2+deg3), CM3)
        
        deg <- as.data.frame(cbind(multilevel, 
                                   bipartiteB1, 
                                   bipartiteB2, 
                                   bipartiteB3,
                                   tripartiteB1B2,
                                   tripartiteB1B3, 
                                   tripartiteB2B3,
                                   tripartiteB1B2B3,
                                   low_multilevel, 
                                   meso_multilevel,
                                   high_multilevel))
        rownames(deg) <- names
      }
      
      if(normalized){
        deg5 <- deg5/n
        deg6 <- deg6/k
        CM1 <- CM1/((n-1)+m+k)
        CM3 <- CM3/((k-1)+n+m)
        
        names <- c(paste(rep("n", dim(A1)[1]), 1:dim(A1)[1], sep=""),
                   paste(rep("m", dim(B1)[2]), 1:dim(B1)[2], sep=""),
                   paste(rep("k", dim(B3)[1]), 1:dim(B3)[1], sep=""))
        multilevel <- c(CM1, L1B1L2B2, CM3)
        bipartiteB1 <- c(deg1, deg2, rep(NA, dim(B3)[1]))
        bipartiteB2 <- c(rep(NA, dim(A1)[1]), deg3, deg4)
        bipartiteB3 <- c(deg6, rep(NA, m), deg5)
        tripartiteB1B2 <- c(deg1, (deg2+deg3), deg4)
        tripartiteB1B3 <- c((deg1+deg6), deg2, deg5)
        tripartiteB2B3 <- c(deg6, deg3, (deg4+deg5))
        tripartiteB1B2B3 <- c((deg1+deg6), (deg2+deg3), (deg4+deg5))
        low_multilevel <- c(CM1, (deg2+deg3), (deg4+deg5))
        meso_multilevel <- c((deg1+deg6), L1B1L2B2, (deg4+deg5))
        high_multilevel <- c((deg1+deg6),  (deg2+deg3), CM3)
        
        deg <- as.data.frame(cbind(multilevel, 
                                   bipartiteB1, 
                                   bipartiteB2, 
                                   bipartiteB3,
                                   tripartiteB1B2,
                                   tripartiteB1B3, 
                                   tripartiteB2B3,
                                   tripartiteB1B2B3,
                                   low_multilevel, 
                                   meso_multilevel,
                                   high_multilevel))
        rownames(deg) <- names
      }
      
      if(!complete){
        
        names <- c(paste(rep("n", dim(A1)[1]), 1:dim(A1)[1], sep=""),
                   paste(rep("m", dim(B1)[2]), 1:dim(B1)[2], sep=""),
                   paste(rep("k", dim(B3)[1]), 1:dim(B3)[1], sep=""))
        multilevel <- c(CM1, L1B1L2B2, CM3)
        deg <- as.data.frame(multilevel)
        rownames(deg) <- names
        
      }
      # TODO: Add multilevel degree for matrices A1, B1 and A3 when B3 are known
    }
    
  }
  if(!dim(deg)[2]==1){
    col_names <- names(deg)
    deg[,col_names] <- lapply(deg[,col_names] , as.character)
    deg[,col_names] <- lapply(deg[,col_names] , as.numeric)
    deg <- round(deg, 3) 
  }
  else{deg <- round(deg, 3) }
  
  return(deg)
}



#' Generalized K-core 
#' 
#' Generalized k-core for undirected, directed, weighted and multilevel networks
#'
#' @param A   A matrix object.
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
#' @import igraph
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
    return(igraph::coreness(g, mode=c(type)))
  }
  if(!multilevel & weighted){
    if(!is.null(B1))stop("Matrix `B1` for multilevel networks")
    W <- A
    ct <- 1
    k.core <- vector("integer", length = nrow(W))
    repeat {
      temp <- gen_degree(W, digraph=digraph, type=type,
                            alpha=alpha,
                            loops=loops, weighted=weighted) 
      threshold <- min(temp[which(temp > 0)])
      v_remove <- which(temp <= threshold & temp > 0)
      if (length(v_remove) > 0) {
        k.core[v_remove] <- ct
        W[v_remove, ] <- W[, v_remove] <- 0
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
      temp <- multilevel_degree(W, B1, 
                                   weightedA1 = weighted,
                                   typeA1 = type, alphaA1 = alpha,
                                   loopsA1 = loops)
      temp <- temp$multilevel
      temp <- head(temp, n=dim(W)[1]) 
      threshold <- min(temp[which(temp > 0)])
      v_remove <- which(temp <= threshold & temp > 0)
      if (length(v_remove) > 0) {
        k.core[v_remove] <- ct
        W[v_remove, ] <- W[, v_remove] <- 0
        B1[v_remove, ] <- 0 # CHECK
        
        ct <- ct + 1
      }
      if (sum(colSums(W) > 0) == 0) 
        break
    }
  }
  return(k.core)
}

