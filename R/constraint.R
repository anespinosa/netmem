#' Print "Hello world" 
#'
#' This is a simple function that, by default, prints "Hello world". You can 
#' customize the text to print (using the \code{to_print} argument) and add
#' an exclamation point (\code{excited = TRUE}).
#'
#' @param to_print A character string giving the text the function will print
#' @param excited Logical value specifying whether to include an exclamation
#'    point after the text
#' 
#' @return This function returns a phrase to print, with or without an 
#'    exclamation point added. As a side effect, this function also prints out
#'    the phrase. 
#'
#' @examples
#' 
#' A <- matrix(c(0,1,1,0,0,1,
#' 1,0,1,0,0,1,
#' 1,1,0,0,0,1,
#' 0,0,0,0,1,1,
#' 0,0,0,1,0,1,
#' 1,1,1,1,1,0), ncol=6, nrow=6,
#' byrow=TRUE)
#' 
#' constraint(A, ego=6)
#' 
#' @export

constraint <- function(A, ego=NULL, digraph=FALSE, weighted=FALSE){
  A <- as.matrix(A)
  A <- ifelse(A>0, 1, 0) # Binarize
  A[lower.tri(A)] = t(A)[lower.tri(A)] # Symmetrize
  diag(A) <- 0
  
  if(dim(A)[1]!=dim(A)[2])stop("Network should be square")
  if(dim(A)[1]<=2 & dim(A)[2]<=2)stop("No constraint for 2x2 network")
  if(class(ego)!="numeric")stop("Label of the name of ego should be in numeric format")
  if(rowSums(A)[ego]!=(dim(A)[1]-1))stop("Ego is not connected with everyone in the network")
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
    A_ego <- A[-ego,-ego] 
    term2 <- (2/(N^2))*sum(rowSums(A_ego)/(rowSums(A_ego)+1))
    require(igraph)
    gA_ego <- igraph::graph.adjacency(A_ego, mode=c("undirected"))
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

