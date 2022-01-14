#' Generalized density
#' 
#' @param A   A symmetric or incident matrix object
#' @param directed    Whether the matrix is directed 
#' @param bipartite   Whether the matrix is bipartite
#' @param loops   Whether to consider the loops
#' @param weighted   Whether the matrix is weighted 
#' @param multilayer   Whether the matrix is multilayer (i.e., multiplex and/or multilevel)
#' 
#' @return This function returns the density of the matrix(es)
#'
#' @author Alejandro Espinosa-Rada
#' 
#' @references 
#' 
#' Wasserman, S., and Faust, K. (1994). Social Network Analysis: Methods and Applications. Cambridge: Cambridge University Press.
#' 
#' @examples
#' 
#' # A bipartite matrix
#' B <- matrix(c(1,1,0,
#'               0,0,1,
#'               0,1,1,
#'               0,0,1), byrow=TRUE, ncol=3)
#' gen_density(B, bipartite = TRUE)
#' 
#' # A multilevel network
#' A1 <- matrix(c(0,1,0,0,1,
#'                1,0,0,1,1,
#'                0,0,0,1,1,
#'                0,1,1,0,1,
#'                1,1,1,1,0), byrow=TRUE, ncol=5)
#' 
#' B1 <- matrix(c(1,0,0,
#'                1,1,0,
#'                0,1,0,
#'                0,1,0,
#'                0,1,1), byrow=TRUE, ncol=3)
#' 
#' A2 <- matrix(c(0,1,1,
#'                1,0,0,
#'                1,0,0), byrow=TRUE, nrow=3)
#' 
#' B2 <- matrix(c(1,1,0,0,
#'                0,0,1,0,
#'                0,0,1,1), byrow=TRUE, ncol=4)
#' 
#' A3 <- matrix(c(0,1,3,1,
#'                1,0,0,0,
#'                3,0,0,5,
#'                1,0,5,0), byrow=TRUE, ncol=4)
#' 
#' matrices <- list(A1, B1, A2, B2, A3)
#' gen_density(matrices, multilayer = TRUE)
#' 
#' # A multiplex network
#' A <- matrix(c(0,1,3,6,4,
#'               2,0,4,5,2,
#'               4,1,0,6,1,
#'               5,6,3,0,6,
#'               1,1,2,3,0), byrow= TRUE, ncol= 5)
#' gen_density(A, multilayer = TRUE)
#' 
#' @export

# TODO: Add density for weighted and multilevel networks
# TODO: Differentiate the class of the elements of the matrix (integral, strings, numeric, ...)

gen_density <- function(A, directed = TRUE, bipartite = FALSE, loops = FALSE,
                        weighted = FALSE, multilayer = FALSE){
  
  if(weighted){
    stop("Density is not yet implemented for weighted networks")
  }else{
    if(!multilayer){
      if(is.list(A)){
        stop("The object should be a matrix")
      }
      matrices <- A
    }
  }
  
  if(multilayer){
    if(is.list(A)){
      for(j in 1:length(A)){
        if(is.matrix(A[[j]]) == FALSE)stop("Not in matrix format")
        if(any(abs(A[[j]])>1))
          warning(paste("The matrix in [[", j ,"]] is valued", sep=''))
      }
      matrices <- A
    }else{
      levels <- unique(sort(c(A)))
      matrices <- list()
      for(i  in levels[levels!=0]){
        matrices[[i]] <- ifelse(A==i, 1, 0)
      }
      names(matrices) <- 1:length(matrices)
      matrices[sapply(matrices, is.null)] <- NULL
    }
  }
  else{
    if(is.list(A))stop("The object should be a matrix")
    if(any(abs(A)>1))stop("The matrix is valued")
  }
  
  if(!loops){
    if(is.list(matrices)){
      for(j in 1:length(matrices)){
        if(ncol(matrices[[j]]) == nrow(matrices[[j]])){
          diag(matrices[[j]]) <- 0  
        }
      }
    }else{
      diag(A) <- 0  
    }
  }
  
  if(is.list(matrices)){
    dens <- list()
    for(j in 1:length(matrices)){
      
      # weighted
      if(any(abs(matrices[[j]])>1)){
        dens[[j]] <- NA
      }else{
        
        # bipartite
        if(ncol(matrices[[j]]) != nrow(matrices[[j]])){
          high <- ncol(matrices[[j]])
          low <- nrow(matrices[[j]])
          L <- sum(matrices[[j]],na.rm=TRUE)
          dens[[j]] <- L/(high*low)
        }else{
          
          # directed or undirected
          if(all(matrices[[j]][lower.tri(matrices[[j]])] == t(matrices[[j]])[lower.tri(matrices[[j]])])){
            dens[[j]] <- sum(matrices[[j]],na.rm=TRUE)/(ncol(matrices[[j]])*(ncol(matrices[[j]])-1))
          }else{
            dens[[j]] <- (sum(matrices[[j]][lower.tri(matrices[[j]])], na.rm=TRUE)*2)/(ncol(matrices[[j]])*(ncol(matrices[[j]])-1))  
          }
        }
      }
      names(dens[[j]]) <- paste("Density of matrix [[", names(matrices)[[j]] ,"]]", sep='')
    }
    
    return(dens)
  }else{
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
}

#' Citation networks
#'
#' Matrix transformation from incident matrices to a citation, fractional counting for co-citation or fractional counting for bibliographic coupling
#'
#' @param A1   From incident matrix of paper and author.
#' @param A2   To incident matrix of author to paper.
#' @param citation    Character string, \dQuote{citation}, \dQuote{cocitation} and \dQuote{bcoupling}
#' 
#' @return Return a type of citation network
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

# TODO: Working progress (expand the measure)

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

#' Co‐occurrence
#' 
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

# TODO: working progress (expand the measure)

coocurrence <- function(OC){
  coOC <- t(OC)%*%OC 
  D <- diag(coOC) 
  coOC/(sqrt(outer(D,D,"*")))
}

#' Jaccard similarity
#' 
#' Jaccard similarity to identify the tie changes between two matrices.
#'
#' @param A  Binary matrix A
#' @param B  Binary matrix B
#' @param directed  Whether the matrix is symmetric 
#' @param diag  Whether the diagonal should be considered
#' @param coparticipation  Select nodes that co-participate in both matrices
#' 
#' @return The output are: \code{jaccard} = Jaccard similarity, \code{proportion} = 
#' proportion among the ties present at a given observation of ties that 
#' are also present in the other matrix, and \code{table} = a table with the 
#' tie changes between matrices.
#' 
#' If \code{coparticipation = TRUE}, then 
#' also: \code{coparticipation1} = percentage 
#' of actors in the first matrix also present in the 
#' second matrix, and \code{coparticipation2}` = percentage 
#' of actors in the second matrix also present in the first matrix.
#'
#' @references
#'
#' Batagelj, V., and Bren, M. (1995). Comparing resemblance measures. Journal of Classification 12, 73–90.
#'
#' @author Alejandro Espinosa-Rada

#' @examples
#' 
#' A <- matrix(c(0,1,1,0,
#'               1,0,0,0,
#'               1,0,0,0,
#'               0,0,1,0), byrow=TRUE, ncol= 4)
#' B <- matrix(c(0,1,1,0,
#'               1,0,0,0,
#'               1,0,0,0,
#'               0,0,0,0), byrow=TRUE, ncol= 4)
#' jaccard(A, B, directed = TRUE)
#' 
#' @export

# TODO: expand for n periods
# TODO: expand for other similarities

jaccard <- function(A, B, directed = TRUE, diag = FALSE,
                    coparticipation = FALSE){
  A <- as.matrix(A)
  B <- as.matrix(B)
  
  if(coparticipation){
    if(all(rownames(A) != colnames(A)))stop("The names of rows and columns does not match")
    if(all(rownames(B) != colnames(B)))stop("The names of rows and columns does not match")
    
    n1t <- ncol(A)
    n2t <- ncol(B)
    name1 <- rownames(A) %in% rownames(B)
    name1 <- rownames(A)[name1 == TRUE]
    A <- A[rownames(A) %in% name1, rownames(A) %in% name1]
    B <- B[rownames(B) %in% name1, rownames(B) %in% name1]
    
    n1 <- ncol(A)
    n2 <- ncol(B)
    
  }
  
  if(any(abs(A>1), na.rm = TRUE))stop("The matrix should be binary")
  if(any(abs(B>1), na.rm = TRUE))stop("The matrix should be binary")
  if(!directed){
    t <- table(A[lower.tri(A, diag=diag)], B[lower.tri(B, diag=diag)])
  }else{
    if(all(A[lower.tri(A)] == t(A)[lower.tri(A)]))warning("The matrix is symmetric")
    A <- c(A[lower.tri(A, diag=diag)],A[upper.tri(A, diag=diag)])
    B <- c(B[lower.tri(B, diag=diag)],B[upper.tri(B, diag=diag)])
    t <- table(A, B, useNA = c("always"))
  }
  n11 <- t[2,2]
  n10 <- t[2,1]
  n01 <- t[1,2]
  n00 <- t[1,1]    
  
  if(coparticipation){
    return(list(jaccard=n11/(n10+n01+n11),
                proportion=n11/(n10+n11),
                table = t,
                coparticipation1 = n1/n1t,
                coparticipation2 = n2/n2t
    ))  
  }else{
    return(list(jaccard=n11/(n10+n01+n11),
                proportion=n11/(n10+n11),
                table = t))
  }
  
}

#' Structural missing data
#' 
#' Assign NA to missing data in the matrices
#'
#' @param A   A symmetric or incident matrix object
#' @param label   String vector with the names of the theoretical complete matrix
#' @param bipartite   Whether the matrix is bipartite or not.
#' @param column   Whether the assignation of NA is for columns in the biparite network, row by default.
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

#' Zone-2 sampling from second-mode
#' 
#' Second-zone multilevel sampling considering a second-mode focal actor
#'
#' @param A   A symmetric matrix object.
#' @param X   X an incident matrix object.
#' @param ego   Whether to add or not ego into the subgraph.
#' @param core  Whether to add actors at distance one from ego
#' 
#' @return This function return a list of second-zone subgraphs using as a focal actor the second-mode of the multilevel network.
#'
#' @references
#'
#' Espinosa-Rada, A. (forthcoming). A Network Approach for the Sociological Study of Science: Modelling Dynamic Multilevel Networks. [PhD]. The University of Manchester.
#'
#' @import igraph
#' 
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' 
#' A <- matrix(c(0,1,0,0,0,0,0,0,
#'               0,0,1,0,0,0,0,0,
#'               0,1,0,1,0,0,0,0,
#'               0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,
#'               0,0,0,1,0,0,0,0,
#'               0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0), byrow=TRUE, ncol=8)
#' colnames(A) <- c("1", "2", "3", "4", "5", "6", "7", "8")
#' rownames(A) <- c("1", "2", "3", "4", "5", "6", "7", "8")
#' 
#' X <- matrix(c(1,0,0,0,
#'               1,0,0,0,
#'               1,0,1,0,
#'               0,1,1,0,
#'               0,1,1,1,
#'               0,1,0,0,
#'               0,0,0,0,
#'               0,0,0,1), byrow=TRUE, ncol=4)
#' colnames(X) <- c("a", "b", "c", "d")
#' rownames(X) <- c("1", "2", "3", "4", "5", "6", "7", "8")
#' 
#' zone_sample(A, X, core=TRUE)
#' 
#' @export

zone_sample <- function(A, X, ego=TRUE, core=FALSE){
  A <- as.matrix(A)
  X <- as.matrix(X)
  if(is.null(rownames(A)))stop("Assign `rownames` to the adjacent matrix")
  if(is.null(colnames(A)))stop("Assign `colnames` to the adjacent matrix")
  if(is.null(rownames(X)))stop("Assign `rownames` to the incident matrix")
  if(is.null(colnames(X)))stop("Assign `colnames` to the incident matrix")
  if(dim(A)[1]!=dim(X)[1]){
    X <- t(X)
  }
  if(!all(colnames(A)==rownames(X)))warning("The names for the combination of the matrices should be the same")
  zero <- matrix(0, ncol=ncol(X), nrow=ncol(X))
  M1 <- cbind(A, X)
  M2 <- cbind(t(X), zero)
  gM <- rbind(M1, M2)
  A1 <- gM
  gM <- igraph::graph.adjacency(gM)
  label <- colnames(X)
  subgraphs <- list()
  zone1 <- list()
  external <- list()
  for(i in label){
    zone1[[i]] <- which(A1[,i]!=0)
    zone2 <- A1%*%A1
    zone2 <- zone2 + A1%*%t(A1)
    nei <- which(zone2[i,]!=0)
    nei <- rownames(as.data.frame(nei))
    nei <- nei[which(nei!=i)]
    not <- names(zone1[[i]]) %in% label
    members_zone1 <- names(zone1[[i]][!not])
    out <- nei[!nei%in%members_zone1]
    out <- out[!out%in%colnames(X)]
    external[[i]] <- length(out)
    outInst <- names(which(X[out,]!=0))
    if(ego==TRUE){
      nei <- c(nei, members_zone1, outInst, i)
    }
    if(ego==FALSE){
      nei <- c(nei, members_zone1, outInst)
    }
    nei <- unique(nei)
    subgraphs[[i]] <- igraph::delete.vertices(gM, !(V(gM)$name %in% nei))
    subgraphs[[i]] <- igraph::simplify(subgraphs[[i]])
    if(core==TRUE){
      V(subgraphs[[i]])$core <- ifelse(V(subgraphs[[i]])$name %in% names(zone1[[i]]),1,0)
    }
  }
  
  return(subgraphs)
}