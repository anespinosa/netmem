# ---------------------------------------------------------------------------- #
# YULE-Q DISTRIBUTION
YuleQDistribution <- function (i, obsData, sims, period, groupName, varName,
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
  n <- length(att)
  
  # ESTIMATE YULE-Q
  same <- abs(outer(att, att, "==")) 
  diff <- abs(outer(att, att, "!=")) 
  same_connected <- ifelse(same & reciprocal ==1, 1, 0)
  same_category <- (rowSums(same)-1)
  I <- rowSums(same_connected, na.rm = TRUE)
  X <- same_category-I
  diff_category <- (rowSums(diff))
  E <- (rowSums(reciprocal, na.rm=TRUE))-I
  Y <- diff_category-E
  a <- ((I*Y)-(E*X))/((I*Y)+(E*X))
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
