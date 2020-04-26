# --------------------------------------------------------------------------------------------- #
### USING UNDERLYING GRAPHS (arcs replaced by non-directed edges)
OverlapTriadCensus <- function (i, obsData, sims, period, groupName, varName, levls = 1:50, 
                                cumulative = FALSE, mode = "max") {
  
  varName1 <- varName[1]
  varName2 <- varName[2]
  
  A <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName1))
  B <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName2))
  D <- A+B
  E <- ifelse((D+t(D))>0, 1, 0) # arcs to edges
  Eb <- ifelse(E==0, 1, 0)
  diag(Eb) <- 0
  M <- ifelse((D+t(D))>1, 1, 0) # mutual edges only
  C <- D-M # asymmetric arcs only
  
  t201=sum(M%*%M*Eb) # null dyad in a 201 triad
  t021D=sum(t(C)%*%C*Eb) # null dyad in an 021D triad  
  t021U=sum(C%*%t(C)*Eb) # null dyad in an 021U triad
  
  res <- c(
    "003" = sum(diag(Eb%*%Eb%*%Eb))/6, # T003
    "012" = sum((Eb%*%Eb)*(C+t(C)))/2, # T012
    "102" = sum((Eb%*%Eb*M))/2, # T102
    "021D" = sum(t(C)%*%C*Eb)/2, # T021D
    "021U" = sum(C%*%t(C)*Eb)/2, # T021U 
    "021C" = sum(C%*%C*Eb), # T021C
    "111D" = (sum(D%*%t(D)*Eb)-t201-t021U)/2, # T111D
    "111U" = (sum(t(D)%*%D*Eb)-t201-t021D)/2, # T111U
    "030T" = sum((C%*%C)*C), # T030T
    "030C" = sum(diag(C%*%C%*%C))/3, # T030C
    "201" = sum(M%*%M*Eb)/2, # T201
    "120D" = sum(t(C)%*%C*M)/2, # T120D
    "120U" = sum(C%*%t(C)*M)/2, # T120U
    "120C" = sum(C%*%C*M), # T120C
    "210" = sum(M%*%M*(C+t(C)))/2, # T210
    "300" = sum(diag(M%*%M%*%M))/6 # T300    
  )
  res
}