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
#' heterogeneity(n, normalized = TRUE)
#' 
#' @export

# TODO: select a better example!

heterogeneity <- function(att, normalized = FALSE){
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


