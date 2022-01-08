#' Transform symmetric matrix to an edge-list
#'
#' @param A   A symmetric matrix
#' @param digraph   Whether the matrix is directed or not
#' @param valued  Add a third columns with the valued of the relationship
#' @param loops   Whether the loops are retained or not
#'
#' @return This function transform the matrix into an edgelist.
#'
#' @importFrom stats aggregate
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(0,2,1,
#'               1,0,0,
#'               1,0,1), byrow=TRUE, ncol=3)
#' matrix_to_edgelist(A, digraph=TRUE, valued = TRUE, loops=TRUE)
#'
#' @export

matrix_to_edgelist <- function(A, digraph=FALSE, valued=FALSE, loops=FALSE){
  M <- A
  
  # if(digraph){
  #   if(sum(M[lower.tri(M)] - M[upper.tri(M)])==0)warning("The network is undirected")
  # }else{
  #   if(abs(sum(M[lower.tri(M)] - M[upper.tri(M)]))>0)warning("The networks might be directed")
  #   M[lower.tri(M)]<-0}
  
  if(is.null(colnames(M))) colnames(M) <- 1:ncol(M)
  if(is.null(rownames(M))) rownames(M) <- 1:nrow(M)
  
  edge <- NULL
  for(i in 1:nrow(M)){
    for(j in 1:ncol(M)){
      if(M[i,j]!=0){
        edge <- c(edge,
                  rep(c(dimnames(M)[[1]][i],
                        dimnames(M)[[2]][j])))
      }
    }
  }
  edge <- matrix(edge, byrow=TRUE, ncol=2)
  
  if(valued){
    edge <- NULL
    for(i in 1:nrow(M)){
      for(j in 1:ncol(M)){
        if(M[i,j]!=0){
          edge <- c(edge,
                    rep(c(dimnames(M)[[1]][i],
                          dimnames(M)[[2]][j]),
                        M[i,j])
          )
          
        }
      }
      
    }
    edge <- matrix(edge, byrow=TRUE, ncol=2)
    df <- as.data.frame(edge)
    edge <- as.matrix(stats::aggregate(list(valued=rep(1,nrow(df))), df, length))
    colnames(edge) <- NULL
  }
  # else{
  #   if(any(A>1))warning("The networks is valued")
  # }
  
  if(loops==FALSE){
    #if(any(diag(M>0)))warning("There are loops in the network")
    edge <- edge[edge[,1]!=edge[,2],]
  }
  
  return(edge)
}

#' Mixing matrix
#' 
#' Create a mixing matrix from node attributes. The mixing matrix is a two-dimensional 
#' matrix that cross-classifies the edges depending on the values of their attributes. 
#' This matrix allowed identifying segregation and homophily at the network level. 
#' 
#' Values in the diagonal are the number of ties within groups, and off-diagonal are the number of relations between groups. 
#'
#' @param A   A symmetric matrix object
#' @param att   Categorical attribute of the nodes
#' 
#' @return This function returns a mixing matrix.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' 
#' n <- 100
#' A <- matrix(c(rbinom(n, 1, 0.5)), 
#'             ncol = sqrt(n), nrow = sqrt(n), byrow = TRUE)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#' att <- rbinom(sqrt(n), 3, 0.5)
#' mixMatrix(A, att=att)
#' 
#' @export
#' 

# TODO: select a better example!

mixMatrix <- function(A, att=NULL){
  if(is.null(att))stop("No attribute has been specified")
  if(is.null(rownames(A)))stop("No label assigned to the rows of the matrix")
  if(is.null(colnames(A)))stop("No label assigned to the columns of the matrix")

  data <- as.data.frame(cbind(label=colnames(A), att=att))
  edgelist <- matrix_to_edgelist(A)
  
  edgeFROM <- as.data.frame(edgelist[,1L])
  colnames(edgeFROM) <- 'label'
  edgeFROM$id <- 1:nrow(edgeFROM)
  edgeFROM <- merge(edgeFROM, data, by = "label")
  edgeFROM <- edgeFROM[order(edgeFROM$id),] 
  
  edgeTO <- as.data.frame(edgelist[,2L])
  colnames(edgeTO) <- 'label'
  edgeTO$id <- 1:nrow(edgeTO)
  edgeTO <- merge(edgeTO, data, by = "label")
  edgeTO <- edgeTO[order(edgeTO$id),] 
  
  mixMATRIX <- do.call(table, c(list(From=edgeFROM$att, 
                                     To=edgeTO$att)))
  
  if(all(A[lower.tri(A)] == t(A)[lower.tri(A)])){
    warning("The network is undirected")
    mixMATRIX <- mixMATRIX + t(mixMATRIX)
    diag(mixMATRIX) <- diag(mixMATRIX)%/%2L 
  }
  return(mixMATRIX)
}

#' Krackhard and Stern's E-I index
#'
#' This index was proposed by Krackhard and Stern (1988) to distinguish between the relative prevalence 
#' of between and within-group ties. This measure can be interpreted as homophily at the network level.
#' 
#' @param A  A symmetric matrix object
#' @param mixed  Whether the matrix provided is already a mixed matrix or not
#' @param att  Categorical attribute of the nodes
#'
#' @return Numerical value of the E-I index.
#'
#' @examples
#' 
#' n <- 100
#' A <- matrix(c(rbinom(n, 1, 0.5)), 
#'             ncol = sqrt(n), nrow = sqrt(n), byrow = TRUE)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#' att <- rbinom(sqrt(n), 3, 0.5)
#' ei.table(A, mixed=FALSE, att=att) 
#' 
#' @export

# TODO: select a better example!

ei.table <- function(A, mixed=TRUE, att=NULL)
{
  if(!mixed){
    matrix <- mixMatrix(A, att)
  }
  if(length(dim(matrix)) == 3)
    m <- matrix[,,2]
  else m <- matrix
  pI <- m / sum(m)
  I <- sum(diag(pI))
  diag(pI) <- 0
  E <- sum(pI)
  EIindex <- E - I
  return( E - I )
}

#' Blau's and IQV Index
#'
#' This index was proposed by Blau (1977) to distinguish between the relative prevalence 
#' of between and within-group ties. This measure can be interpreted as homophily at the network level.
#' 
#' @param att  Categorical attribute of the nodes
#' @param normalized  Whether to return IQV index
#'
#' @return Numerical value of the Blau index and the IQV index.
#' 
#' @references 
#' 
#' Agresti, A. and Agresti, B. (1978). Statistical Analysis of Qualitative Variation. Sociological Methodology, 9, 204-237. doi: \url{https://doi.org/10.2307/270810}   
#'
#' Blau, P. M. (1977). Inequality and heterogeneity. New York: Free Press.
#' 
#' @examples
#' 
#' n <- 100
#' att <- rbinom(sqrt(n), 3, 0.5)
#' blau(n, normalized = TRUE)
#' 
#' @export

# TODO: select a better example!

blau <- function(att, normalized = FALSE){
  att <- as.character(att)
  p <- (table(att)/sum(table(att)))^2
  r <- length(p)
  blau <- 1
  for(i in 1:r){
    blau <- blau - p[i]  
  }
  if(normalized){
    iqv <- blau/(1-1/r)
    return(list(blau = blau, iqv = iqv))
  }else(return(blau))
}


