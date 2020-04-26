### DIRECTED MODULARITY
rm(list=ls())
g <- graph_from_literal( 1--2, 4--5, 3--1, 5--2, 4--3 )
m1 <- as_adj(g, sparse = FALSE) # the order of the matrix is not from 1:5






membership <- c(1,2,1,2,1)
#m1 <- m1[order(rownames(m1)),
#         order(colnames(m1))]

Q <- function(x, member, directed=FALSE, diagonal = T){
  
  x <- as.matrix(x)
  m <- sum(m1)/2 # n ties for undirected
  
  groups <- outer(member, member, "==") 
  if(!diagonal) diag(groups) <- 0 
  
  # the formula
  if(!directed){
    p <- outer(rowSums(x),rowSums(x),"*")/(2*m)
    q <- (1/(2*m))*sum(groups*(x-p), na.rm = T) 
  }
  
  if(directed){ #Arenas et al. (2007) (out-in)
    p <- outer(rowSums(x),colSums(x),"*")/(m)
    q <- (1/(m))*sum(groups*(x-p), na.rm = T) 
  }
  
  # pagerank
  
  # gaussian
  
  return(q)
}
Q(m1, membership)
Q(m1, membership, diagonal=F)
Q(m1, membership, directed = TRUE)
Q(m1, membership, directed = TRUE, diagonal=F)
Q(m1, membership, directed = TRUE, diagonal=T)


# --------------------------------------------------------------------------------------------- #
# https://www.researchgate.net/post/Can_anyone_provide_a_short_example_of_how_the_modularity_is_being_calculated_in_networks
library(igraph)
g <- graph_from_literal( 1--2, 4--5, 3--1, 5--2, 4--3 )

#g <- graph_from_literal( 1--2, 4--5, 3--1, 5--2, 4--3, 5-3 ) # TEST1

plot(g)
m1 <- as_adj(g, sparse = FALSE) # the order of the matrix is not from 1:5
m1 <- m1[order(rownames(m1)),
               order(colnames(m1))]
membership <- c(1,2,1,2,1)
q <- modularity(g, membership)

Q <- function(x, group, diag = T){

  x <- as.matrix(x)
  m <- sum(m1)/2 # n ties for undirected
  
  same.group <- outer(group, group, "==") 
  if(!diag) diag(same.group) <- 0 
  
  p <- outer(rowSums(a),rowSums(a),"*")/(2*m)
  
  # the formula
  q <- (1/(2*m))*sum(same.group*(a-p), na.rm = T)
  return(q)
}
Q(g, membership)

# --------------------------------------------------------------------------------------------- #


# https://github.com/igraph/rigraph/issues/331

# Recently, I have discovered a default setting in the modularity function, 
# that I find problematic. The function “assumes” that nodes can 
# also have self-ties. I know this might be sometimes the case, 
# but for networks where modularity is applied on (e.g., Karate data) 
# self-ties are often not possible and actually dont make sense 
# (a community can only be between people and not within people). 
# Because the diagonal in a “same group” matrix is 1, 
# whereas it should be 0, the returned modularity score is biased. 
# Here is some R-Code that demostrates this:

library(igraph)
library(igraphdata)

data(karate)
Q <- function(x, group, diag = T){
  m <- length(E(x)) # n ties
  
  same.group <- outer(group, group, "==") 
  if(!diag) diag(same.group) <- 0 
  
  a <- as.matrix(as_adjacency_matrix(x))
  p <- outer(rowSums(a),rowSums(a),"*")/(2*m)
  
  # the formula
  q <- (1/(2*m))*sum(same.group*(a-p), na.rm = T)
  return(q)
}
Q(g, membership)


# --------------------------------------------------------------------------------------------- #

# if each node is in its own community, the modularity should be zero, but it is not:
modularity(karate, 1:length(V(karate)))
Q(karate, 1:length(V(karate)))
# now it is zero:
Q(karate, 1:length(V(karate)), diag = F)

# Q = 1 / 2m * sum( (Aij - ki*kj / (2m) ) delta(ci,cj), i, j)
# m is the number of links = 5
# Aij is the element (i, j) of the adjacency matrix depicted above
# ki, kj are the degrees of nodes i, j (always 2 in your example, note that all row/column sums of the adjacency matrix are 2)
# and delta(ci,cj) is the Kronecker delta, which is 1 if nodes i and j are in the same module and 0 otherwise. 
# In this way, Q only cares about links that fall within modules and ignores those which link separate modules to each other (except as a normalizing factor).
# To calculate Q, just go through each element of the adjacency matrix. 
# Note that if it is not within a module (that is, the nodes defined 
# by the row and the column have different membership values), 
# then that element of the sum will be multiplied by 0, so you 
# can save some time by only considering links within the modules. 
# For example, the first row of the adjacency matrix yields:


#Q = 1 / (2 * 5) ( (0 - 2 * 2 / (2 * 5) ) * 1 +     # node 1 to 1 -> absent, same membership
#                    (1 - 2 * 2 / (2 * 5) ) * 0 +     # node 1 to 2 -> present, different membership
#                    (0 - 2 * 2 / (2 * 5) ) * 1 +     # node 1 to 4 -> absent, same membership
#                    (0 - 2 * 2 / (2 * 5) ) * 0 +     # node 1 to 5 -> present, different membership
#                    (1 - 2 * 2 / (2 * 5) ) * 1 +     # node 1 to 3 -> present, same membership
#                    ...
#                  Continuing for the rest of the matrix eventually simplifies to:
#                    Q = 1 / 10 ( 7 * (0 - 2 / 5) + 6 * (1 - 2 / 5) ) = 4 / 50 = 0.08


# edgelist:
#as.data.frame(as.table(m1))
edgelist <- data.frame(from=rownames(m1)[row(m1)], 
                       to=colnames(m1)[col(m1)],
                       values=c(m1))

# membership matrix
mem <- abs(outer(membership,membership,"=="))
rownames(mem) <- 1:dim(mem)[1]
colnames(mem) <- 1:dim(mem)[1]
mem_edgelist <- data.frame(from=rownames(mem)[row(mem)], 
                           to=colnames(mem)[col(mem)],
                           member=c(mem))

edgelist_comp <- as.data.frame(cbind(edgelist, delta=mem_edgelist$member))

m <- sum(m1)/2 # for undirected
kij <- max(membership)
g <- dim(m1)[1]

out <- rowSums(m1) # outdegree
ind <- colSums(m1) # indegree
edgelist_comp$out <- rep(out, g)
edgelist_comp$ind <- rep(ind, g)

edgelist_comp$to <- as.numeric(as.character(edgelist_comp$to))
edgelist_comp$from <- as.numeric(as.character(edgelist_comp$from))
edgelist_comp$from2 <- edgelist_comp$from


edgelist_comp$sum <- (edgelist_comp$values-edgelist_comp$out*edgelist_comp$ind/(2*m))*edgelist_comp$delta
1/(2*m)*sum(edgelist_comp$sum) # this is correct! But, it would show a different results if the matrix is ordered different
q

#edgelist_comp[c(c),]


edgelist_comp$sum <- (edgelist_comp$values-edgelist_comp$out*edgelist_comp$ind/(2*m))*edgelist_comp$delta
1/(2*m)*sum(edgelist_comp$sum)

