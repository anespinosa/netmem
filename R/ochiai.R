rm(list=ls())

# -------------------------------------------------------------------------- #
### NORMALIZATION WITH SALTON COSINE
## Direct citation considering loop
D <- diag(Ci_aD)
S_CiD <- Ci_aD/(sqrt(outer(D,D,"*")))

## Bibliographic coupling considering loop
D <- diag(biCoD)
S_biCoD <- biCoD/(sqrt(outer(D,D,"*")))

## Co-citation considering loop
D <- diag(coCiD)
S_coCiD <- coCiD/(sqrt(outer(D,D,"*")))

### NEED FURTHER EXPLORATION: Ochiai is better than COSINE according to
# Zhou and Leydesdorff
# The Ochiai coefficients based on the minimum overlap 
# function can be formalized as
# Ochiai(x,y)=\Sum_{ni}min(x_{i},y_{i})/sqrt(\Sum_{i}^{n}=1 x_{I} \Sum_{i}^{n}=1 y_{I})�𝑖=1 min(𝑥𝑖,𝑦𝑖)

#library(ade4)
#dist.binary
df <- Ci_aD
a <- df %*% t(df)
b <- df %*% (1 - t(df))
c <- (1 - df) %*% t(df)
d <- ncol(df) - a - b - c

### Ochiai
# https://cstheory.stackexchange.com/questions/14736/minimizing-the-maximum-dot-product-among-k-unit-vectors-in-an-n-dimensional-spac
M <- matrix(c(3,1,2,
              1,6,5,
              2,5,8), byrow=T, nrow=3, ncol=3)

df <- M
a <- df %*% t(df)
b <- df %*% (1 - t(df))
c <- (1 - df) %*% t(df)
d <- ncol(df) - a - b - c
df/sqrt((a + b) * (a + c))
a/sqrt((a + b) * (a + c))


D <- diag(df)
df/(sqrt(outer(D,D,"*")))
a/(sqrt(outer(D,D,"*")))


# Zhou & Leydesdorff 2015
# TABLE 1
OC <- matrix(c(2,0,2,
               1,1,0,
               0,3,3,
               0,2,2,
               0,0,1), 
             nrow=5, ncol=3, byrow = TRUE)
OC


# TABLE 2
coOC <- t(OC)%*%OC
coOC

# TABLE 3
M <- matrix(c(3,1,2,
              1,6,5,
              2,5,8), byrow=T, nrow=3, ncol=3)
M
D <- diag(1/colSums(OC))
1/(sqrt(outer(D,D,"*")))


# THIS IS CORRECT FOR OCCHIAI NORMALIZATION AND SHOULD BE USED!!
D <- diag(coOC) 
coOC/(sqrt(outer(D,D,"*")))







df <- OC
a <- df %*% t(df)
b <- df %*% (1 - t(df))
c <- (1 - df) %*% t(df)
d <- ncol(df) - a - b - c
a/sqrt((a + b) * (a + c))

df <- coOC
a <- df %*% t(df)
b <- df %*% (1 - t(df))
c <- (1 - df) %*% t(df)
d <- ncol(df) - a - b - c
(df/sqrt((a + b) * (a + c)))*2
a/sqrt((a + b) * (a + c))

a
D <- diag(df)
df/(sqrt(outer(D,D,"*"))) # inner product (cosine)


a/(sqrt(outer(D,D,"*")))


D <- diag(df)
df/(sqrt(outer(D,D,"*")))
