# ---------------------------------------------------------------------- #
### MIXED INDEGREE FOR THREE-LEVELS
Mixed3LDegreeIn <- function (i, obsData, sims, period, groupName, varName, levls=c(seq(0,0.3,by=0.05)), 
                             cumulative = TRUE) {
  
  if (length(varName) != 3) stop("Mixed3LDegreeIn expects three varName parameters")
  
  varName1 <- varName[1]
  varName2 <- varName[2]
  varName3 <- varName[3]
  
  A <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName1))
  B <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName2))
  C <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName3))
  
  if ((dim(A)[1] != dim(A)[2]) | (dim(A)[1] != dim(B)[1]) | (dim(A)[1] != dim(C)[1])) stop("Error: The first element in varName must be one-mode, the second and the third two-mode")
  
  # Size of the networks
  n <- dim(A)[1]
  m <- dim(B)[2]
  k <- dim(C)[2]
  
  # Multilevel degree measures
  m1 <- cbind(array(0, dim=c(dim(B)[2], dim(B)[2])),
              t(B), array(0, dim=c(dim(B)[2], dim(C)[2])))
  m2 <- cbind(B, A, C)
  m3 <- cbind(array(0, dim=c(dim(C)[2], dim(C)[2])),
              t(C), array(0, dim=c(dim(C)[2], dim(B)[2])))
  
  a <- c(margin.table(m1,2)/(n),
         margin.table(m2,2)/(m+k+(n-1)),
         margin.table(m3,2)/(n))
  
  lel <- length(levls)
  if (cumulative)
  {
    cdi <- sapply(2:lel, function(i){sum(a<=levls[i])})
  }
  else
  {
    cdi <- sapply(2:lel, function(i){
      sum(a<=levls[i]) - sum(a <= levls[i-1])})
  }
  names(cdi) <- as.character(levls[2:lel])
  cdi
}

# ---------------------------------------------------------------------- #
### MIXED OUTDEGREE FOR THREE-LEVELS
Mixed3LDegreeOut <- function (i, obsData, sims, period, groupName, varName, levls=c(seq(0,0.4,by=0.05)), 
                              cumulative = TRUE) {
  
  if (length(varName) != 3) stop("Mixed3LDegreeOut expects three varName parameters")
  
  varName1 <- varName[1]
  varName2 <- varName[2]
  varName3 <- varName[3]
  
  A <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName1))
  B <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName2))
  C <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName3))
  
  if ((dim(A)[1] != dim(A)[2]) | (dim(A)[1] != dim(B)[1]) | (dim(A)[1] != dim(C)[1])) stop("Error: The first element in varName must be one-mode, the second and the third two-mode")
  
  # Size of the networks
  n <- dim(A)[1]
  m <- dim(B)[2]
  k <- dim(C)[2]
  
  # Multilevel degree measures
  m1 <- cbind(array(0, dim=c(dim(B)[2], dim(B)[2])),
              t(B), array(0, dim=c(dim(B)[2], dim(C)[2])))
  m2 <- cbind(B, A, C)
  m3 <- cbind(array(0, dim=c(dim(C)[2], dim(C)[2])),
              t(C), array(0, dim=c(dim(C)[2], dim(B)[2])))
  
  a <- c(margin.table(m1,1)/(n),
         margin.table(m2,1)/(m+k+(n-1)),
         margin.table(m3,1)/(n))
  
  lel <- length(levls)
  if (cumulative)
  {
    cdi <- sapply(2:lel, function(i){sum(a<=levls[i])})
  }
  else
  {
    cdi <- sapply(2:lel, function(i){
      sum(a<=levls[i]) - sum(a <= levls[i-1])})
  }
  names(cdi) <- as.character(levls[2:lel])
  cdi
}

## MIXED TWO-LEVELS OUTDEGREE
Mixed2LDegreeOut <- function (i, obsData, sims, period, groupName, varName, levls=c(seq(0,0.4,by=0.05)), 
                              cumulative = TRUE) {
  
  if (length(varName) != 2) stop("Mixed2LDegree expects two varName parameters")
  
  varName1 <- varName[1]
  varName2 <- varName[2]
  
  A <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName1))
  B <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName2))
  
  if ((dim(A)[1] != dim(A)[2])) stop("Error: The first element in varName must be one-mode, the second two-mode")
  
  # Size of the networks
  n <- dim(A)[1]
  m <- dim(B)[2]
  
  # Multilevel degree measures
  m1 <- cbind(array(0, dim=c(dim(B)[2], dim(B)[2])),
              t(B))
  m2 <- cbind(B, A)
  
  a <- c(margin.table(m1,1)/(n),
         margin.table(m2,1)/(m+(n-1)))
  
  lel <- length(levls)
  if (cumulative)
  {
    cdi <- sapply(2:lel, function(i){sum(a<=levls[i])})
  }
  else
  {
    cdi <- sapply(2:lel, function(i){
      sum(a<=levls[i]) - sum(a <= levls[i-1])})
  }
  names(cdi) <- as.character(levls[2:lel])
  cdi
}

# ---------------------------------------------------------------------- #
## MIXED TWO-LEVELS INDEGREE
Mixed2LDegreeIn <- function (i, obsData, sims, period, groupName, varName, levls=c(seq(0,0.4,by=0.05)), 
                             cumulative = TRUE) {
  
  if (length(varName) != 2) stop("Mixed2LDegreeb expects two varName parameters")
  
  varName1 <- varName[1]
  varName2 <- varName[2]
  
  A <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName1))
  B <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName2))
  
  if ((dim(A)[1] != dim(A)[2])) stop("Error: The first element in varName must be one-mode, the second two-mode")
  
  # Size of the networks
  n <- dim(A)[1]
  m <- dim(B)[2]
  
  # Multilevel degree measures
  m1 <- cbind(array(0, dim=c(dim(B)[2], dim(B)[2])),
              t(B))
  m2 <- cbind(B, A)
  
  a <- c(margin.table(m1,2)/(n),
         margin.table(m2,2)/(m+(n-1)))
  
  lel <- length(levls)
  if (cumulative)
  {
    cdi <- sapply(2:lel, function(i){sum(a<=levls[i])})
  }
  else
  {
    cdi <- sapply(2:lel, function(i){
      sum(a<=levls[i]) - sum(a <= levls[i-1])})
  }
  names(cdi) <- as.character(levls[2:lel])
  cdi
}

# ---------------------------------------------------------------------- #
### One-mode and bipartite networks
## MIXED TWO-LEVELS OUTDEGREE
Mixed2LDegreeOut <- function (i, obsData, sims, period, groupName, varName, levls=c(seq(0,0.4,by=0.05)), 
                              cumulative = TRUE) {
  
  if (length(varName) != 2) stop("Mixed2LDegree expects two varName parameters")
  
  varName1 <- varName[1]
  varName2 <- varName[2]
  
  A <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName1))
  B <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName2))
  
  if ((dim(A)[1] != dim(A)[2])) stop("Error: The first element in varName must be one-mode, the second two-mode")
  
  # Size of the networks
  n <- dim(A)[1]
  m <- dim(B)[2]
  
  # Multilevel degree measures
  m1 <- cbind(array(0, dim=c(dim(B)[2], dim(B)[2])),
              t(B))
  m2 <- cbind(B, A)
  
  a <- c(margin.table(m1,1)/(n),
         margin.table(m2,1)/(m+(n-1)))
  
  lel <- length(levls)
  if (cumulative)
  {
    cdi <- sapply(2:lel, function(i){sum(a<=levls[i])})
  }
  else
  {
    cdi <- sapply(2:lel, function(i){
      sum(a<=levls[i]) - sum(a <= levls[i-1])})
  }
  names(cdi) <- as.character(levls[2:lel])
  cdi
}

# ---------------------------------------------------------------------- #
## MIXED TWO-LEVELS INDEGREE
Mixed2LDegreeIn <- function (i, obsData, sims, period, groupName, varName, levls=c(seq(0,0.4,by=0.05)), 
                             cumulative = TRUE) {
  
  if (length(varName) != 2) stop("Mixed2LDegreeb expects two varName parameters")
  
  varName1 <- varName[1]
  varName2 <- varName[2]
  
  A <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName1))
  B <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName2))
  
  if ((dim(A)[1] != dim(A)[2])) stop("Error: The first element in varName must be one-mode, the second two-mode")
  
  # Size of the networks
  n <- dim(A)[1]
  m <- dim(B)[2]
  
  # Multilevel degree measures
  m1 <- cbind(array(0, dim=c(dim(B)[2], dim(B)[2])),
              t(B))
  m2 <- cbind(B, A)
  
  a <- c(margin.table(m1,2)/(n),
         margin.table(m2,2)/(m+(n-1)))
  
  lel <- length(levls)
  if (cumulative)
  {
    cdi <- sapply(2:lel, function(i){sum(a<=levls[i])})
  }
  else
  {
    cdi <- sapply(2:lel, function(i){
      sum(a<=levls[i]) - sum(a <= levls[i-1])})
  }
  names(cdi) <- as.character(levls[2:lel])
  cdi
}