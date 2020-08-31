#' Generalized density
#' 
#'
#' @param A   A symmetric or incident matrix object
#' @param directed    Wheter the matrix is directed or not
#' @param bipartite   Wheter the matrix is bipartite or not.
#' @param loops   Wheter to consider or not the loops
#' 
#' @return This function returns the density of the matrix.
#'
#' @author Alejandro Espinosa-Rada
#' 
#' @references 
#' 
#' Wasserman, S., and Faust, K. (1994). Social Network Analysis: Methods and Applications. Cambridge: Cambridge University Press.
#' 
#' @examples
#' 
#' B <- matrix(c(1,1,0,
#'               0,0,1,
#'               0,1,1,
#'               0,0,1), byrow=TRUE, ncol=3)
#' gen_density(B, bipartite = TRUE)
#' 
#' 
#' @export

# TO DO: ADD DENSITY FOR WEIGHTED AND MULTILEVEL NETWORKS

gen_density <- function(A, directed=TRUE, bipartite=FALSE ,loops=FALSE){
  if(!loops){
    diag(A) <- 0
  }
  if(bipartite){
    if(dim(A)[1]==dim(A)[2])warning("Incident matrix should be rectangular")
    high <- ncol(A)
    low <- nrow(A)
    L <- sum(A,na.rm=TRUE)
    dens <- L/(high*low)
  }
  else{
    if(!dim(A)[1]==dim(A)[2])stop("Matrix should be square")
    
    if(directed){
      if(all(A[lower.tri(A)] == t(A)[lower.tri(A)]))warning("The network is undirected")
      dens <- sum(A,na.rm=TRUE)/(ncol(A)*(ncol(A)-1))
    }
    if(!directed){
      if(!all(A[lower.tri(A)] == t(A)[lower.tri(A)]))warning("The network is directed. The underlying graph is used")
      A[lower.tri(A)] = t(A)[lower.tri(A)] # Symmetrize
      dens <- (sum(A[lower.tri(A)], na.rm=TRUE)*2)/(ncol(A)*(ncol(A)-1))}
  }
  return(dens)
}

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

# TO DO: working progress (expand the measure)

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

#' Co‐occurrence matrix based on overlap function 
#'
#' @param OC  Asymmetrical occurrence matrix
#' 
#' @return This function returns the symmetrical co‐occurrence matrix using the minimal overlap
#'
#' @references
#'
#' Zhou, Q., & Leydesdorff, L. (2016). The normalization of occurrence and Co-occurrence matrices in bibliometrics using Cosine similarities and Ochiai coefficients. Journal of the Association for Information Science and Technology, 67(11), 2805–2814. \url{https://doi.org/10.1002/asi.23603}
#'
#' @author Alejandro Espinosa-Rada

#' @examples
#' 
#' OC <- matrix(c(2,0,2,
#'                1,1,0,
#'                0,3,3,
#'                0,2,2,
#'                0,0,1), 
#'              nrow=5, byrow = TRUE)
#' 
#' coocurrence(OC)
#' 
#' @export

# TO DO: working progress (expand the measure)

coocurrence <- function(OC){
  coOC <- t(OC)%*%OC 
  D <- diag(coOC) 
  coOC/(sqrt(outer(D,D,"*")))
}

#' Structural missing data
#' 
#' Assign NA to missing data in the matrices
#'
#' @param A   A symmetric or incident matrix object
#' @param label   String vector with the names of the theoretical complete matrix
#' @param bipartite   Wheter the matrix is bipartite or not.
#' @param column   Wheter the assignation of NA is for columns in the biparite network, row by default.
#' 
#' @return This function returns NA to missing data.
#'
#' @author Alejandro Espinosa-Rada

#' @examples
#' 
#' A <- matrix(c(0,1,1,
#'               1,0,1,
#'               0,0,0), byrow=TRUE, ncol=3)
#' colnames(A) <- c("A", "C", "D")
#' rownames(A) <- c("A", "C", "D")
#' label <- c("A", "B", "C", "D", "E")
#' structuralNA(A, label)
#' 
#' @export

structuralNA <- function(A, label=NULL, bipartite=FALSE, column=FALSE){
  if(bipartite){
    if(dim(A)[1]==dim(A)[2])warning("Incident matrix should be rectangular")
    if(column){
      for(i in 1:dim(A)[2]){ 
        A[,i] = ifelse((A[,i] | sum(A[,i])) == 0, 
                       NA, A[,i])
      }
      x <- A
      
    }
    else{
      for(i in 1:dim(A)[1]){ 
        A[i,] = ifelse((A[i,] | sum(A[i,])) == 0, 
                       NA, A[i,])
      }
      x <- A
    }
    return(x)
  }
  else{
    if(!dim(A)[1]==dim(A)[2])stop("Matrix should be square")
    if(is.null(colnames(A)))stop("Assign column names to the matrix.")
    if(is.null(rownames(A)))stop("Assign column names to the matrix.")
    if(!is.character(label))stop("Assign a string vector with the names of the complete matrix.")
    x <- array(NA, dim=list(length(label),length(label)))
    colnames(x) <- label
    rownames(x) <- label
    rowmatch <- match(rownames(A), rownames(x))
    colmatch <- match(colnames(A), colnames(x))
    x[rowmatch, colmatch] <- A
  }
  return(x)
}
