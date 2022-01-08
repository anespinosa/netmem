#' Ego network
#' 
#' Submatrix of ego's neighbourhoods
#'
#' @param A   A symmetric matrix object
#' @param ego   Name of ego in the matrix
#' @param bipartite  Whether the matrix is a two-mode network
#' @param addEgo  Whether to retain ego in the submatrix or not
#' @param select   Whether to consider all sender and receiver ties of ego (\code{all}), only incoming ties (\code{in}), or outgoing ties (\code{out}). By default, \code{all}. 
#' 
#' @return This function returns redundancy, effective size and efficincy measures (Burt, 1992).
#'
#' @references
#'
#' Burt, R.S., 1992. Structural Holes: the Social Structure of Competition. Harvard University Press, Cambridge.
#' 
#' Borgatti, S., 1997. Unpacking Burt's redundancy measure. Connections, 20(1): 35-38. doi: \url{http://www.analytictech.com/connections/v20(1)/holes.htm}     
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' 
#' A <- matrix(c(0,1,0,0,1,1,1,
#'               1,0,0,1,0,0,1,
#'               0,0,0,0,0,0,1,
#'               0,1,0,0,0,0,1,
#'               1,0,0,0,0,0,1,
#'               1,0,0,0,0,0,1,
#'               1,1,1,1,1,1,0), ncol = 7, byrow = TRUE)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#' ego_net(A, ego = "g")
#' 
#' @export


ego_net <- function(A, ego=NULL, bipartite=FALSE, addEgo=FALSE,
                    select = c('all', 'in', 'out')){
  
  if(class(ego)=="numeric")stop("Label of the name of ego should be in character format")
  if(is.null(rownames(A)))stop("No label assigned to the rows of the matrix")
  if(is.null(colnames(A)))stop("No label assigned to the columns of the matrix")
  if(!(ego %in% sort(unique(c(rownames(A), colnames(A))))))stop("Ego name does not match with the names of the enlisted nodes")
  
  A <- as.matrix(A)
  ego <- as.character(ego)
  # select <- switch(node_direction(select), "out"=1, "in"=2, "all"=3)
  select <- switch(node_direction(select), "out"=1, "in"=2, "all"=3)
  
  if(bipartite==TRUE){
    if(ncol(A)==nrow(A))warning("Matrix should be rectangular")
    E <- matrix(0, nrow=nrow(A), ncol=nrow(A), byrow=TRUE)
    rownames(E) <- rownames(A)
    colnames(E) <- rownames(A)
    S <- matrix(0, nrow=ncol(A), ncol=ncol(A), byrow=TRUE)
    rownames(S) <- colnames(A)
    colnames(S) <- colnames(A)
    SE <- t(A)
    UP <- cbind(A, E)
    DOWN <- cbind(S, SE)
    A <- rbind(UP, DOWN)
  }
  
  if(is.null(rownames(A))){
    rownames(A) <- as.character(1:nrow(A))
  }
  if(is.null(rownames(A))){
    colnames(A) <- as.character(1:nrow(A))
  }
  
  # In, out or all
  if(select == 1){
    name <- names(which(A[ego,]!=0))
  }
  if(select == 2){
    name <- names(which(A[,ego]!=0))
  }
  if(select == 3){
    nameOut <- names(which(A[ego,]!=0))
    nameIn <- names(which(A[,ego]!=0))  
    name <- unique(c(nameOut, nameIn))
  }
  
  if(addEgo){
    subA <- A[name, name]
    # A[ego,][A[ego,]!=0]
    addEGO <- rbind(subA, A[ego,][c(name)])
    # c(A[,ego][A[,ego]!=0],0)
    addEGO <- cbind(addEGO, A[,ego][c(name,ego)])
    
    rownames(addEGO) <- c(rownames(addEGO)[rownames(addEGO) %in% name], ego)
    colnames(addEGO) <- c(colnames(addEGO)[colnames(addEGO) %in% name], ego)
    
    return(addEGO)
  }else{return(A[name, name])}
  
}

node_direction <- function(arg, choices, several.ok=FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }
  
  arg <- tolower(arg)
  choices <- tolower(choices)
  
  match.arg(arg=arg, choices=choices, several.ok=several.ok)
}

#' Redundancy Measures
#' 
#' Redundancy measures of the structural holes theory for binary matrixes
#'
#' @param A   A symmetric matrix object
#' @param ego   Name of ego in the matrix
#' @param digraph   Whether the  matrix is directed or undirected
#' @param weighted  Whether the matrix is weighted or not
#' 
#' @return This function returns redundancy, effective size and efficincy measures (Burt, 1992).
#'
#' @references
#'
#' Burt, R.S., 1992. Structural Holes: the Social Structure of Competition. Harvard University Press, Cambridge.
#' 
#' Borgatti, S., 1997. Unpacking Burt's redundancy measure. Connections, 20(1): 35-38. doi: \url{http://www.analytictech.com/connections/v20(1)/holes.htm}     
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' 
#' A <- matrix(c(0,1,0,0,1,1,1,
#'               1,0,0,1,0,0,1,
#'               0,0,0,0,0,0,1,
#'               0,1,0,0,0,0,1,
#'               1,0,0,0,0,0,1,
#'               1,0,0,0,0,0,1,
#'               1,1,1,1,1,1,0), ncol = 7, byrow = TRUE)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#' burt(A, ego = "g")
#' 
#' @export

# TODO: DIRECTED AND WEIGHTED CASES

burt <- function(A, ego=NULL, digraph=FALSE, weighted=FALSE){
  A <- ego_net(A, ego = ego)
  A <- ifelse(A>0, 1, 0) # Binarize
  A[lower.tri(A)] = t(A)[lower.tri(A)] # Symmetrize
  diag(A) <- 0
  
  if(digraph==TRUE)stop("Measure only implemented for binary and undirected networks,
                        for directed networks it would use the underlying graph")
  if(weighted==TRUE)stop("Measure only implemented for binary and undirected networks,
                        for directed networks it would use the underlying graph")
  if(digraph==TRUE & weighted==TRUE)stop("Measure only implemented for binary and undirected networks,
                        for directed networks it would use the underlying graph")
  
  if(digraph==FALSE){
  redundancy <- mean(rowSums(A))
  effective_size <- ncol(A) - redundancy
  efficiency <- effective_size/ncol(A) 
  return(list(redundancy=redundancy,
              effective_size=effective_size, 
              efficiency=efficiency))
  }
}

#' Constraint
#' 
#' Everett and Borgatti specification of the constraint measure for binary matrixes
#'
#' @param A   A symmetric matrix object
#' @param ego   Name of ego in the matrix
#' @param digraph   Whether the  matrix is directed or undirected
#' @param weighted  Whether the matrix is weighted or not
#' 
#' @return This function returns term 1, 2 and 3, the normalization and the maximum value of the specification of Everett and Borgatti (2020),
#' and the constraint of Burt (1992).
#'
#' @references
#'
#' Burt, R.S., 1992. Structural Holes: the Social Structure of Competition. Harvard University Press, Cambridge.
#' 
#' Everett, M.G. and Borgatti, S., 2020. Unpacking Burt's constraint measure. Social Networks 62, pp. 50-57. doi: \url{https://doi.org/10.1016/j.socnet.2020.02.001}
#'
#' @import igraph
#' 
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' 
#' A <- matrix(c(0,1,1,0,0,1,
#'              1,0,1,0,0,1,
#'              1,1,0,0,0,1,
#'              0,0,0,0,1,1,
#'              0,0,0,1,0,1,
#'              1,1,1,1,1,0), ncol=6, byrow=TRUE)
#' 
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- letters[1:ncol(A)]
#' eb_constraint(A, ego='f')
#' 
#' @export

eb_constraint <- function(A, ego=NULL, digraph=FALSE, weighted=FALSE){
  A <- ego_net(A, ego = ego, addEgo=TRUE)
  A <- as.matrix(A)
  A <- ifelse(A>0, 1, 0) # Binarize
  A[lower.tri(A)] = t(A)[lower.tri(A)] # Symmetrize
  diag(A) <- 0
  
  if(dim(A)[1]!=dim(A)[2])stop("Matrix should be square")
  if(dim(A)[1]<=2 & dim(A)[2]<=2)stop("No constraint for 2x2 network")
  
  if(class(ego)=="numeric")stop("Label of the name of ego should be in character format")
  if(is.null(rownames(A)))stop("No label assigned to the rows of the matrix")
  if(is.null(colnames(A)))stop("No label assigned to the columns of the matrix")
  
  if(digraph==TRUE)stop("Measure only implemented for binary and undirected networks,
                        for directed networks it would use the underlying graph")
  if(weighted==TRUE)stop("Measure only implemented for binary and undirected networks,
                        for directed networks it would use the underlying graph")
  if(digraph==TRUE & weighted==TRUE)stop("Measure only implemented for binary and undirected networks,
                        for directed networks it would use the underlying graph")
  
  if(digraph==FALSE){
    output <- matrix(ncol=5, nrow=1)
    N <- rowSums(A)[ego] 
    term1 <- (1/N) 
    A_ego <- A[rownames(A)!=ego,]
    A_ego <- A_ego[,colnames(A_ego)!=ego]
    term2 <- (2/(N^2))*sum(rowSums(A_ego)/(rowSums(A_ego)+1))
    gA_ego <- graph.adjacency(A_ego, mode=c("undirected"))
    pq <- list()
    for(q in 1:ncol(A_ego)){
      q1 <- as.vector(igraph::neighborhood(gA_ego)[[q]]) 
      q1 <- rowSums(A_ego)[q1]
      q1 <- q1[-1]
      pq[[q]] <- q1 
    }
    pq[sapply(pq, function(pq) length(pq)==0)] <- NULL
    x <- 0
    for(j in 1:length(pq)){
      x = x + sum(1/(unlist(pq[[j]])+1))^2
    }
    term3 <- (1/N^2)*x
    constraint <- term1+term2+term3
    
    if(dim(A)[1]<8){
      maximum <- (((2*N)-1)^2)/(N^3)  
    }
    else{maximum <- (((N+1)^2)*((N^2)+(4*N)-4))/(4*N^4)}
    normalization <- (constraint-term1)/(maximum-term1)
    output <- cbind(term1, term2, term3, constraint, normalization)
  }
  label <- c("term1", "term2", "term3", "constraint", "normalization")
  output <- as.data.frame(output)
  output <- round(output, 3)
  colnames(output) <- label
  maximum <- round(maximum, 3)
  newlist <- list(results=output, maximum=maximum)
  return(newlist)
  
}
