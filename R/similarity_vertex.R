# ---------------------------------------------------------------------------- #
# SIMILRITY DISTRIBUTION
SimilarityDistribution <- function (i, obsData, sims, period, 
                                    groupName, varName, normalize=TRUE,
                                    levls=NULL, cumulative=TRUE){
  
  varName1 <- varName[1]
  varName2 <- varName[2]
  
  x <- as.matrix(sparseMatrixExtraction(i, obsData, sims, 
                                        period, groupName, 
                                        varName = varName1))
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
  
  if(is.null(obsData[[groupName]]$vCovars[[varName2]])){
    stop(paste("There is no changing individual covariates name", varName2))
    
  }
  
  att <- obsData[[groupName]]$vCovars[[varName2]][,period]
  distance <- abs(outer(att, att, "-")) 
  
  if(dim(distance) != dim(reciprocal)){
    stop("The matrixes has different dimensions.")
  }
  
  distance <- ifelse(reciprocal & distance >=0, 
                     distance, NA)
  
  if(normalize)
  {
    a <- 1-(abs(distance)/(max(distance, na.rm=T)-min(distance, na.rm=T)))
    a <- (rowSums(a,1, na.rm = T)/rowSums(reciprocal,1, na.rm = T))
    a[is.na(a)] <- Inf
  }
  else
  {
    a <- 1-distance
    a <- rowSums(a,1, na.rm = T)
    a[is.na(a)] <- Inf
  }
  
  if (cumulative)
  {
    
    lel <- length(levls)
    cdi <- sapply(2:lel, function(i){sum(a<=levls[i])})
    
  }
  else
  {
    
    lel <- length(levls)
    
    cdi <- sapply(2:lel, function(i){
      sum(a<=levls[i]) - sum(a <= levls[i-1])}) 
    
  }
  names(cdi) <- as.character(levls[2:lel])
  cdi
}