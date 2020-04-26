# --------------------------------------------------------------------- #
#### Batagelj and Cerinsek 2013  
## Newman 2001
## Normalization of the bipartite collaboration network
Newman <- WA/(rowSums(WA)-1)

#### Batagelj and Cerinsek 2013  
## Normalization of the bipartite collaboration network
N <- WA/rowSums(WA)

## PENDING: Row-normalization n(setN)

### FIRST collaboration network
# number of works that authors i and j wrote together.
AW <- t(WA)
Co <- AW%*%WA
table(rowSums(Co)==colSums(Co))
# Options for the weighted network:
# Salton’s cosine similarity (Salton 1987) 
# Ochiai coefficient between authors i and j

# The obvious question is: who are the most collaborative authors?
# The standard answer is provided by k-cores 
# (Batagelj and Zaversnik 2011).

### SECOND collaboration network
# To neutralize the overrating of the contribution of works 
# with many authors
# Interpretation: contribution of author j to works, 
# that he/she wrote together with the author i
Cn <- AW %*%N


### THIRD collaboration network
# Interpretation: total contribution of collaboration 
# of authors i and j to works
Ct <- t(N)%*%N

# --------------------------------------------------------------------- #
## On fractional approach to analysis of linked networks
# Vladimir Batagelj (2020)
# Fraction coauthors
WA <- matrix(c(1,0,1,0,
               1,1,0,0,
               1,0,1,1,
               0,1,0,1,
               1,0,1,1), nrow=5, ncol=4, byrow = TRUE)

WK <- matrix(c(1,1,0,0,
               1,0,1,0,
               0,1,1,1,
               0,0,1,0,
               0,1,0,1), nrow=5, ncol=4, byrow = TRUE)

t(WA)%o%WK

# Outer product decomposition
H <- t(WA)%*%WK

H1 <- WA[1,]%o%WK[1,]
H2 <- WA[2,]%o%WK[2,]
H3 <- WA[3,]%o%WK[3,]
H4 <- WA[4,]%o%WK[4,]
H5 <- WA[5,]%o%WK[5,]

H1+H2+H3+H4+H5

# Fractional approach
SwWA <- ifelse(rowSums(WA)>0, rowSums(WA), 1)
norm1 <- diag(1/SwWA) # diagonal normalization matrices

SwWK <- ifelse(rowSums(WK)>0, rowSums(WK), 1)
norm2 <- diag(1/SwWK) # diagonal normalization matrices

WAn <- norm1%*%WA
WKn <- norm2%*%WK

# --------------------------------------------------------------------- #
### Perianes-Rodrigueza, Waltmanb & van Eck, 2016
# TABLE 2
A <- matrix(c(1,1,0,
               1,0,1,
               1,1,0,
               0,0,1), 
             nrow=4, ncol=3, byrow = TRUE)

# TABLE 3a
CO_b <- A%*%t(A)
diag(CO_b) <- 0
CO_b

# TABLE 3b
PWvE <- t(A)/(colSums(A)-1)
PWvE <- A%*%PWvE
diag(PWvE) <- 0
PWvE

# TABLE 4
B <- matrix(c(3,1,2,0,
              2,0,1,0,
              1,2,0,0,
              0,0,0,1,
              0,1,0,1), 
            nrow=5, ncol=4, byrow = TRUE)

# TABLE 5a
biCo_b <- B%*%t(B)
diag(biCo_b) <- 0
biCo_b

# TABLE 5b
PWvE_2 <- t(B)/(colSums(B)-1)
PWvE_2 <- B%*%PWvE_2
diag(PWvE_2) <- 0
PWvE_2


# --------------------------------------------------------------------- #
### Leydesdorff & Park, 2017
## TABLE 1
CO <- matrix(c(1,1,0,
               1,0,1,
               1,1,0,
               0,0,1
               ), nrow=4, ncol=3, byrow = TRUE)

## TABLE 2
CO_b <- CO%*%t(CO)
diag(CO_b) <- 0
CO_b

cbind(CO[,1]/colSums(CO)[1],
      CO[,2]/colSums(CO)[2],
      CO[,3]/colSums(CO)[3])

## TABLE 3
t(t(CO)/(rowSums(CO)))

## TABLE 4 
PWvE_l <- t(CO)/(colSums(CO)-1)
PWvE_l <- CO%*%PWvE_l
diag(PWvE_l) <- 0
PWvE_l

## TABLE 5
CO_c <- t(t(CO)/(colSums(CO)))
CO_c%*%t(CO_c)

### TABLE 6?? (directed networks!)
#a <- CO/c(t(t(colSums(CO))))
#b <- t(CO)/(rowSums(CO))
#(a*2)%*%b


Ci <- AK%*%KB 
WA%*%Ci%*%t(WA)

t(t(CO))*2

c(t(colSums(CO)))*(rowSums(CO)-1)

a <- CO/c(t(t(colSums(CO))))
b <- t(CO)/(rowSums(CO))

CO/(rowSums(CO)-1)

(a*2)%*%b

# denominator: n_jk * (n_ik-1)
test <- (t(t(CO))*2)/c(t(colSums(CO))*(colSums(CO)-1))
test%*%t(test)

test2 <- (t(t(CO))*2)/c(t(colSums(CO))*(colSums(CO)-1))
test2%*%t(test2)

CO
CO^1

(colSums(CO)-1)

(t(t(CO))*2)/(colSums(CO)*(colSums(CO)-1))

CO_c <- t(t(CO))/(t(rowSums(CO))*(colSums(CO)-1))
CO_c%*%t(CO_c)


PWvE_l2 <- t(CO)/colSums(CO)*(rowSums(CO)-1)
PWvE_l2 <- CO%*%PWvE_l2
diag(PWvE_l2) <- 0
PWvE_l2

N <- diag(1/rowSums(CO))
Ct <- t(N)%*%N # he contributions of authors to their works.
N_p <- diag(1/rowSums(CO-1))%*%CO
Ct_p <- t(N)%*%(N_p^2)
  
Ct_p%*%t(Ct_p)

N_p%*%CO

t(CO)%*%N

CO/rowSums(CO)

numerator <- CO%*%t(CO)*2
denominator <- CO/colSums(CO)*(CO/rowSums(CO)-1)

numerator/denominator

test%*%t(test)



# --------------------------------------------------------------------- #
## On fractional approach to analysis of linked networks
# Vladimir Batagelj (2020)
Ci <- matrix(c(3,1,2,0,
              2,0,1,0,
              1,2,0,0,
              0,0,0,1,
              0,1,0,1), nrow=5, ncol=4, byrow = TRUE)

D <- diag(1/rowSums(Ci))
Cin <- D%*%Ci
biC <- Cin%*%t(Ci)


Sx <- rowSums(Ci)/sum(Ci)
Sy <- colSums(Ci)/sum(Ci)



D <- diag(1/(Ci*l)-l)

diag((t(Ci)*l)-l)


D <- diag(1/l)
Cin <- Ci%*%D
biC <- Cin%*%t(Ci)
biC

D <- diag(1/colSums(Ci))
Cin <- Ci%*%D
biC <- Cin%*%t(Ci)


head(sequence)
ABC <- cbind(paste("to", as.numeric(organizations$to_author), sep=""),
             as.character(organizations$from_author)) # projection on from_author
ABC <- unique(ABC) # avoid multiple citations of the same author?!
gABC <- graph.data.frame(ABC, directed = FALSE)
V(gABC)$type <- ifelse(V(gABC)$name %in% ABC[,1], TRUE, FALSE)
gABC <- bipartite.projection(gABC)$proj1
gABC <- igraph::union(gABC, empty)

# bibliographic coupling
ABC <- cbind(as.character(sequence$from_author), 
             as.character(sequence$to_paper))
gABC <- graph.data.frame(ABC, directed = FALSE)
V(gABC)$type <- ifelse(V(gABC)$name %in% ABC[,1], TRUE, FALSE)

iABC <- get.incidence(gABC, sparse = FALSE)
fABC <- iABC/(rowSums(iABC)-1) # fraction counting bibliographic coupling
fABC <- ifelse(fABC == Inf, 1, fABC) # created because -1!!
fABC <- ifelse(is.na(fABC), 0, fABC)


### 
M <- matrix(c(3,1,2,0,
              2,0,1,0,
              1,2,0,0,
              0,0,0,1,
              0,1,0,1), nrow=5, ncol=4, byrow = TRUE)
M
FC <- M%*%t(M)
diag(FC) <- 0
FC

M

Md <- M%*%t(M)


Mn <- M%*%t(M) # numerator

ifelse(Mn>=1, 1, 0)



diag(Mn) <- 0
null1 <- matrix(rep(rowSums(M),dim(M)[1]), nrow = dim(M)[1], 
                byrow = TRUE)

null2 <- matrix(rep(colSums(M),dim(M)[1]), nrow = dim(M)[1], 
                byrow = TRUE)

null1
null2

rownames(M) <- c("R1", "R2", "R3", "R4", "R5")
colnames(M) <- c("P1", "P2", "P3", "P4")

library(tilting)
projection(M, active = colSums(M))

edgelist <- get.edgelist(graph.adjacency(M %*% t(M)))
edgelist <- unique(edgelist)

edgelist

null2 <- matrix(rep(colSums(M),dim(M)[1]), nrow = dim(M)[1], 
                byrow = TRUE)
null3 <- ifelse(M>=1, null2, 0)
null4 <- null3%*%t(null3)

M

something <- M * null3
something%*%t(something)


l <- c(1,1,1,1)
diag <- ((t(M))-l)^-l
diag(diag) <-0 
M%*%diag(diag)


null4 <- ifelse(null4>=1, 1, 0)



abs(outer(M, null3, FUN="=="))



M


ifelse(Mn>=1)

colSums(M)

gI <- graph.incidence(M)


abs(outer(rowSums(M), rowSums(M), FUN="=="))

abs(outer(rowSums(M),rowSums(M),"+"))

diag(t(M)*rowSums(M))

t(M)*rowSums(M)

test <- M%*%((t(M))-1)^-1

test1 <- M*diag((t(M)*colSums(M)-1)^-1)
test1%*%t(test1)

Md/null

md2 <- null
md2%*%t(md2)

array(colSums(M), 
      dim=dim(M))


(M%*%t(M))/2

Md <- ifelse(M>=1, 1, 0)
Md <- Md%*%t(Md)
diag(Md) <-0

colSums(M)

Md%*%t(Md)/(rowSums(Md))



null <- ifelse(M%*%t(M)>=1,1, 0)
diag(null) <- 0

rowSums(M)/colSums(M)

(M/(colSums(M)-1))%*%t(M)
(M %*% t(M))/(rowSums(M)-1)


fM <- M/(colSums(M)-1)
fM <- ifelse(fM == Inf, 1, fM) # created because -1!!
fM <- ifelse(is.na(fM), 0, fM)

margin.table(M,1)
margin.table(M,2)

fM%*%t(fM)

(1/(t(M)+1)-1)%*%M
M%*%diag((1/(t(M)+1)-1))%*%t(M)


d <- diag(t(M)*1-1)^-1
d <- ifelse(d==Inf, 0, d)
1/(M*d)%*%t(M)

d1 <- M%*%d
d1%*%t(M)
