#' Ochiai and Cosine similarities
#' 
#' Ochiai and Cosine similarities for occurrence and co-occurrence matrices.
#' 
#' @param OC  Matrix
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

rm(list=ls())
### TOY EXAMPLE (LEYDESDORFF)
A <- matrix(c(2,0,2,
              1,1,0,
              0,3,3,
              0,2,2,
              0,0,1), byrow=TRUE, ncol=3)

### ADD OVERLAPPING FOR MIN AND MAX
overlap <- function(A, row=TRUE, min=TRUE){
  if(row==FALSE){
    A <- t(A) 
  }
  sim.jac <- matrix(0, nrow=nrow(A), ncol=nrow(A))
  rownames(sim.jac) <- rownames(A)
  colnames(sim.jac) <- rownames(A)
  pairs <- t(combn(1:nrow(A), 2))
  for (i in 1:nrow(pairs)){
    if(min){
      num <- sum(sapply(1:ncol(A), function(x)(min(A[pairs[i,1],x],A[pairs[i,2],x]))))
    }
    else{
      num <- sum(sapply(1:ncol(A), function(x)(max(A[pairs[i,1],x],A[pairs[i,2],x]))))
    }
    sim.jac[pairs[i,1],pairs[i,2]] <- num
    sim.jac[pairs[i,2],pairs[i,1]] <- num  
  }
  sim.jac[which(is.na(sim.jac))] <- 0
  diag(sim.jac) <- rowSums(A)
  return(sim.jac)  
}
overlap(A, row=FALSE)
overlap(A, row=TRUE)
overlap(A, row=FALSE, min=TRUE)
overlap(A, row=FALSE, min=FALSE)
overlap(A, row=TRUE, min=TRUE)
overlap(A, row=TRUE, min=FALSE)

####### TABLE 4
D <- colSums(A)
IN <- (t(A)%*%A)
OVER <- overlap(A, row=FALSE)

### Occurrence matrix
# COSINE
(t(A)%*%A)/(sqrt(outer(colSums(A^2),colSums(A^2),"*")))

# OCHIAI
Di <- rowSums(A) 
Dj <- colSums(A) 
Ab <- (t(A)%*%A)
diag(Ab) <- colSums(A) # impute diagonal
Ab/(sqrt(outer(Dj,Dj,"*")))

### Co-occurrence matrix based on inner product
# COSINE:
INb <- IN
Di <- rowSums(INb^2)
Dj <- colSums(INb^2)
(IN%*%t(IN))/(sqrt(outer(Di,Dj,"*")))

# OCHIAI:
Di <- rowSums(A^2)
Dj <- colSums(A^2)
IN/(sqrt(outer(Dj,Dj,"*")))

### Co-occurrence matrix based on overlap function/OCHIAI
# COSINE:
OVERb <- OVER
Di <- rowSums(OVERb^2)
Dj <- colSums(OVERb^2)
OVER%*%t(OVER)/(sqrt(outer(Di,Dj,"*")))

# OCHIAI:
D <- colSums(A)
OVER/(sqrt(outer(D,D,"*")))


#### NETMEM IMPLEMENTATION ####
coocurrence <- function(OC){
  coOC <- t(OC)%*%OC 
  D <- diag(coOC) 
  coOC/(sqrt(outer(D,D,"*")))
}

#### NEW IMPLEMENTATION ####
OC <- matrix(c(2,0,2,
              1,1,0,
              0,3,3,
              0,2,2,
              0,0,1), byrow=TRUE, ncol=3)

coocurrence <- function(OC, 
                        coocurrence=TRUE,
                        similarity=c("cosine", "ochiai")){
  
  # SQUARE
  if(coocurrence==TRUE){
    if(ncol(OC)!=nrow(OC))stop("Adjacency matrix should be square")
    
  }
  
  # RECTANGULAR
  if(!coocurrence){
    if(ncol(OC)==nrow(OC))warning("Incident matrix should be rectangular")
    
    
  }
}

coocurrence(OC)
coocurrence(OC, coocurrence = TRUE)
coocurrence(OC, coocurrence = FALSE)
coocurrence(OC%*%t(OC))
coocurrence(OC%*%t(OC), coocurrence = TRUE)
coocurrence(OC%*%t(OC), coocurrence = FALSE)



