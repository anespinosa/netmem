# ---------------------------------------------------------------------------- #
# E-I INDEX DISTRIBUTION
EIndexDistribution <- function (i, obsData, sims, period, groupName, varName,
                                levls=NULL, cumulative=TRUE){
  
  varName1 <- varName[1]
  varName2 <- varName[2]
  
  if(is.null(obsData[[groupName]]$cCovars[[varName2]])){
    stop(paste("There is no constant individual covariates name", varName2))
    
  }
  
  x <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName1))
  if(dim(x)[1]==dim(x)[2])
  {
    reciprocal <- x * t(x)
    reciprocal <- as.matrix(reciprocal)
  }
  else
  { 
    x <- x %*% t(x)
    diag(x) <- 0
    x <- ifelse(x>=1, 1, 0)
    
    reciprocal <- x * t(x)
    reciprocal <- as.matrix(reciprocal)
  }
  
  att <- obsData[[groupName]]$cCovars[[varName2]]
  
  # change the name of the matrix in here
  same <- abs(outer(att, att, "==")) 
  same <- as.matrix(same)
  
  # ESTIMATING THE E-I INDEX
  same <- ifelse(reciprocal & same ==1, 1, 0)
  
  I <- rowSums(same, na.rm = TRUE)
  E <- (rowSums(reciprocal, na.rm=TRUE))-I
  
  a <- (E-I)/(E+I)
  a[is.na(a)] <- Inf 
  
  if (cumulative)
  {
    
    lel <- length(levls)
    cdi <- sapply(1:lel, function(i){sum(a<=levls[i])})
    
  }
  else
  {
    
    lel <- length(levls)
    
    cdi <- sapply(1:lel, function(i){
      sum(a<=levls[i]) - sum(a <= levls[i-1])})
    
  }
  names(cdi) <- as.character(levls[1:lel])
  cdi
  
}