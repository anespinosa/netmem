# ------------------------------------------------------------------------ #
### LIMITED BUT CORRECT!
rm(list=ls())
library(igraph)

# TRIVIAL NETWORKS:
A <- matrix(c(0,1,
              1,0,
              1,1), ncol=2, nrow=3, byrow=TRUE)
A <- matrix(c(0,1,
              1,0), ncol=2, nrow=2, byrow=TRUE)
A <- matrix(c(0,1,1,
              1,0,1,
              1,1,0), ncol=3, nrow=3, byrow=TRUE)

# Example Fig 1a
A <- matrix(c(0,1,1,0,0,1,
              1,0,0,0,0,1,
              1,0,0,0,0,1,
              0,0,0,0,0,1,
              0,0,0,0,0,1,
              1,1,1,1,1,0), ncol=6, nrow=6,
            byrow=TRUE)
gA <- graph.adjacency(A, mode=c("undirected"))
plot(gA)

# Example Fig 1b
A <- matrix(c(0,1,0,0,0,1,
              1,0,0,0,0,1,
              0,0,0,1,0,1,
              0,0,1,0,0,1,
              0,0,0,0,0,1,
              1,1,1,1,1,0), ncol=6, nrow=6,
            byrow=TRUE)

# Example Fig 2a
A <- matrix(c(0,1,1,0,0,1,
              1,0,1,0,0,1,
              1,1,0,0,0,1,
              0,0,0,0,1,1,
              0,0,0,1,0,1,
              1,1,1,1,1,0), ncol=6, nrow=6,
            byrow=TRUE)

# Example Fig 2b
A <- matrix(c(0,1,1,0,0,1,
              1,0,0,1,0,1,
              1,0,0,0,1,1,
              0,1,0,0,0,1,
              0,0,1,0,0,1,
              1,1,1,1,1,0), ncol=6, nrow=6,
            byrow=TRUE)

A <- matrix(c(0,1,1,0,0,1,
              1,0,0,1,0,1,
              1,0,0,0,1,1,
              0,1,0,0,0,1,
              0,0,1,0,0,1,
              1,1,1,1,1,0), ncol=6, nrow=6,
            byrow=TRUE)
#rownames(A) <- c("A", "B", "C", "D", "E", "F")
#colnames(A) <- c("A", "B", "C", "D", "E", "F")


# TO DO: EXPAND TO DIRECTED AND VALUED NETWORKS
constraint <- function(A, ego=NULL, digraph=FALSE, weighted=FALSE){
  A <- as.matrix(A)
  
  # It follows that in a directed network with no reciprocated ties 
  # we can simply ignore the direction and apply formula (3) 
  # on the underlying graph. For a network with all reciprocated 
  # ties we could again ignore the directions and use the 
  # underlying graph and we would get the same result.
  
  A <- ifelse(A>0, 1, 0) # Binarize the network
  A[lower.tri(A)] = t(A)[lower.tri(A)] # Symmetrize the network 
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
    output <- matrix(ncol=5, nrow=1) #dim(A)[1]
    N <- rowSums(A)[ego] 
    term1 <- (1/N) 
    A_ego <- A[-ego,-ego] 
    
    # Given an ego i and an unreciprocated alter j then pij = 1/ ρ(i) 
    # where ρ(i) is the total degree of i ie in-degree plus out-degree. 
    # For a reciprocated tie p = 2/ ρ(i).
    # M <- ifelse((A+t(A))>1, 1, 0) # mutual edges only
    # C <- A-M # asymmetric arcs only
    
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
    #normalization <- ifelse(normalization==Inf, 0, normalization)
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
constraint(A, ego=6)
#constraint(A, 1)
#constraint(A, "A")
#constraint(A, ego=6, digraph=TRUE)
#constraint(A, ego=6, weighted=TRUE)
#constraint(A, ego=6, digraph=TRUE, weighted=TRUE)
constraint(A, ego=2)

constraint(gA)

# Data with a mixture of reciprocated and unreciprocated 
# ties needs to take full account of the reciprocity of each tie.

# ------------------------------------------------------------------------ #
### WRONG!!
rm(list=ls())
library(igraph)
# Example Fig 1a
A <- matrix(c(0,1,1,0,0,1,
              1,0,0,0,0,1,
              1,0,0,0,0,1,
              0,0,0,0,0,1,
              0,0,0,0,0,1,
              1,1,1,1,1,0), ncol=6, nrow=6,
            byrow=TRUE)
gA <- graph.adjacency(A, mode=c("undirected"))
plot(gA)


constraint <- function(A, digraph=FALSE, weighted=FALSE){
  A <- as.matrix(A)
  A[lower.tri(A)] = t(A)[lower.tri(A)] # Symmetrize the matrix 
  diag(A) <- 0
  if(digraph==FALSE){
    output <- matrix(ncol=5, nrow=dim(A)[1])
    for(i in 1:dim(A)[1]){
    N <- rowSums(A)[i] 
    term1 <- (1/N) 
    A_ego <- A[-i,-i] 
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
    normalization <- ifelse(normalization==Inf, 0, normalization)
    output[i,] <- cbind(term1, term2, term3, constraint, normalization)
    }
    
    if(digraph==TRUE)stop("Measure only implemented for binary and undirected networks")
    if(weighted==TRUE)stop("Measure only implemented for binary and undirected networks")
    
    
  }
  label <- c("term1", "term2", "term3", "constraint", "normalization")
  output <- as.data.frame(output)
  output <- round(output, 3)
  colnames(output) <- label
  maximum <- round(maximum, 3)
  newlist <- list(results=output, maximum=maximum)
  return(newlist)
  
}

# WRONG FOR THE OTHER CASES, ONLY CORRECT=6
constraint(A)

# ------------------------------------------------------------------------ #
### WRONG!!!
rm(list=ls())
library(igraph)
# Example Fig 1a
A <- matrix(c(0,1,1,0,0,1,
              1,0,0,0,0,1,
              1,0,0,0,0,1,
              0,0,0,0,0,1,
              0,0,0,0,0,1,
              1,1,1,1,1,0), ncol=6, nrow=6,
            byrow=TRUE)
gA <- graph.adjacency(A, mode=c("undirected"))
plot(gA)

constraint <- function(A, digraph=FALSE, weighted=FALSE){
  require(igraph)
  A <- as.matrix(A)
  A[lower.tri(A)] = t(A)[lower.tri(A)] # Symmetrize the matrix 
  diag(A) <- 0
  if(digraph==FALSE){
    output <- matrix(ncol=5, nrow=dim(A)[1])
    for(i in 1:dim(A)[1]){
      
      N <- rowSums(A)[i] 
      term1 <- (1/N) 
      term1 <- ifelse(is.na(term1), 0, term1) # when actors are isolated
      
      A_ego <- A[-i,-i] 
      # ERROR: N is different when i is not connected to everyone
      
      #pj <- ifelse(dim(A_ego)[1]<1, 0, rowSums(A_ego))
      
      #term2 <- (2/(N^2))*sum(pj/(pj+1))
      term2 <- (2/(N^2))*sum(rowSums(A_ego)/(rowSums(A_ego)+1))
      term2 <- ifelse(is.na(term2), 0, term2)
      gA_ego <- igraph::graph.adjacency(A_ego, mode=c("undirected"))
      
      if(ncol(A_ego)>1){
        pq <- list()
        for(q in 1:ncol(A_ego)){
          q1 <- as.vector(neighborhood(gA_ego)[[q]]) 
          
          q1 <- ifelse(dim(A_ego)[1]<1, 0, rowSums(A_ego)[q1])
          #q1 <- rowSums(A_ego)[q1] # error
          
          
          q1 <- q1[-1]
          pq[[q]] <- q1 
        }
        pq[sapply(pq, function(pq) length(pq)==0)] <- NULL
        if(length(pq)>1){
          
          x <- 0
          for(j in 1:length(pq)){
            x = x + sum(1/(unlist(pq[[j]])+1))^2 # here is the error=not all has +1
          }
          term3 <- (1/N^2)*x
        }
        else{term3 <- 0}
      }
      
      constraint <- term1+term2+term3
      
      if(dim(A)[1]<8){
        maximum <- (((2*N)-1)^2)/(N^3)  
      }
      else{maximum <- (((N+1)^2)*((N^2)+(4*N)-4))/(4*N^4)}
      normalization <- (constraint-term1)/(maximum-term1)
      normalization <- ifelse(normalization==Inf, 0, normalization)
      output[i,] <- cbind(term1, term2, term3, constraint, normalization)
    }
    
    if(digraph==TRUE)stop("Measure only implemented for binary and undirected networks")
    if(weighted==TRUE)stop("Measure only implemented for binary and undirected networks")
    
    
  }
  label <- c("term1", "term2", "term3", "constraint", "normalization")
  output <- as.data.frame(output)
  output <- round(output, 3)
  colnames(output) <- label
  maximum <- round(maximum, 3)
  newlist <- list(results=output, maximum=maximum)
  return(newlist)
  
}

# WRONG FOR THE OTHER CASES, ONLY CORRECT=6
constraint(A)

#gA <- graph.adjacency(A, mode=c("undirected"))
#V(gA)$names <- 1:dim(A)[1]
#gAs <- induced.subgraph(graph=gA, 
#                        vids=unlist(neighborhood(graph=gA,
#                                                 order=1,
#                                                 nodes=V(gA)$names[i])))
#A <- get.adjacency(gAs, sparse=FALSE)

