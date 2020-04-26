MixedLayerTriadCensus <- function (i, obsData, sims, period, groupName, varName, levls = 1:50, 
                                   cumulative = FALSE, mode = "max") {
  
  varName1 <- varName[1]
  varName2 <- varName[2]
  
  A <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName1))
  B <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName2))
  
  E <- ifelse((A+t(A))>0, 1, 0) # arcs to edges
  Eb <- ifelse(E==0, 1, 0)
  diag(Eb) <- 0
  M <- ifelse((A+t(A))>1, 1, 0) # mutual edges only
  C <- A-M # asymmetric arcs only
  
  t201=sum(M%*%M*Eb) # null dyad in a 201 triad
  t021D=sum(t(C)%*%C*Eb) # null dyad in an 021D triad  
  t021U=sum(C%*%t(C)*Eb) # null dyad in an 021U triad
  sum(diag(Eb%*%Eb%*%Eb))/6 # 003
  
  D <- A+B
  D <- ifelse(D>=1, 1, 0) # UNWEIGHTED???
  E3 <- ifelse((D+t(D))>0, 1, 0) # arcs to edges
  Eb3 <- ifelse(E3==0, 1, 0)
  diag(Eb3) <- 0
  M3 <- ifelse((D+t(D))>1, 1, 0) # mutual edges only
  C3 <- D-M3 # asymmetric arcs only
  
  t201b=sum(M3%*%M3*Eb3) # null dyad in a 201 triad
  t021Db=sum(t(C3)%*%C3*Eb3) # null dyad in an 021D triad  
  t021Ub=sum(C3%*%t(C3)*Eb3) # null dyad in an 021U triad
  
  res <- c(
    
    # TRIAD 003
    "003_003" = sum(diag(Eb%*%Eb%*%Eb))/6+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T003 in matrix A
    "003_102" = sum(diag(Eb%*%Eb%*%Eb))/6+sum((Eb3%*%Eb3*M3))/2, # T003+T102-Overlap
    "003_201" = sum(diag(Eb%*%Eb%*%Eb))/6+sum(M3%*%M3*Eb3)/2, # T003+T201-Overlap
    "003_300" = sum(diag(Eb%*%Eb%*%Eb))/6+sum(diag(M3%*%M3%*%M3))/6, # T003+T300-Overlap
    
    # TRIAD 012
    "012_003" = sum((Eb%*%Eb)*(C+t(C)))/2+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T012 in matrix A
    "012_102" = sum((Eb%*%Eb)*(C+t(C)))/2+sum((Eb3%*%Eb3*M3))/2, # T012+T102-Overlap
    "012_111d" = sum((Eb%*%Eb)*(C+t(C)))/2+(sum(D%*%t(D)*Eb3)-t201b-t021Ub)/2, # T012+111D-Overlap
    "012_111u" = sum((Eb%*%Eb)*(C+t(C)))/2+(sum(t(D)%*%D*Eb3)-t201b-t021Db)/2, # T012+111U-Overlap
    "012_201" = sum((Eb%*%Eb)*(C+t(C)))/2+sum(M3%*%M3*Eb3)/2, # T012+T201-Overlap
    "012_210" = sum((Eb%*%Eb)*(C+t(C)))/2+sum(M3%*%M3*(C3+t(C3)))/2, # T012+T210-Overlap
    "012_300" = sum((Eb%*%Eb)*(C+t(C)))/2+sum(diag(M3%*%M3%*%M3))/6,
    
    # TRIAD 021U
    "021u_003" = sum(C%*%t(C)*Eb)/2+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T021u in matrix A
    "021u_021u" = sum(C%*%t(C)*Eb)/2+sum(C3%*%t(C3)*Eb3)/2, # T021u+T021U-Overlap
    "021u_120u" = sum(C%*%t(C)*Eb)/2+sum(C3%*%t(C3)*M3)/2, # T021u+T120U-Overlap
    "021u_210" = sum(C%*%t(C)*Eb)/2+sum(M%*%M*(C+t(C)))/2, # T021d+210-Overlap
    "021u_300" = sum(C%*%t(C)*Eb)/2+sum(diag(M3%*%M3%*%M3))/6, # T021c+300-Overlap
    
    # TRIAD 021D
    "021d_003" = sum(t(C)%*%C*Eb)/2+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T021d in matrix A
    "021d_111u" = sum(t(C)%*%C*Eb)/2+(sum(t(D)%*%D*Eb3)-t201b-t021Db)/2, # T021d+111U-Overlap
    "021d_120d" = sum(t(C)%*%C*Eb)/2+sum(t(C3)%*%C3*M3)/2, # T021d+120d-Overlap
    "021d_210" = sum(t(C)%*%C*Eb)/2+sum(M3%*%M3*(C3+t(C3)))/2, # T021d+210-Overlap
    "021d_300" = sum(t(C)%*%C*Eb)/2+sum(diag(M3%*%M3%*%M3))/6, # T021c+300-Overlap
    
    # TRIAD 102
    "102_003" = sum((Eb%*%Eb*M))/2+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # 102 in matrix A
    "102_102" = sum((Eb%*%Eb*M))/2+sum((Eb3%*%Eb3*M3))/2, # T102+102-Overlap
    "102_201" = sum((Eb%*%Eb*M))/2+sum(M3%*%M3*Eb3)/2, # T102+201-Overlap
    "102_300" = sum((Eb%*%Eb*M))/2+sum(diag(M3%*%M3%*%M3))/6, # T102+300-Overlap
    
    # TRIAD 021C
    "021c_003" = sum(C%*%C*Eb)+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T021c in matrix A
    "021c_111d" = sum(C%*%C*Eb)+(sum(D%*%t(D)*Eb3)-t201b-t021Ub)/2, # T021c+111D-Overlap
    "021c_111u" = sum(C%*%C*Eb)+(sum(t(D)%*%D*Eb3)-t201b-t021Db)/2, # T021c+111U-Overlap
    "021c_120c" = sum(C%*%C*Eb)+sum(C3%*%C3*M3), # T021c+T120c-Overlap
    "021c_201" = sum(C%*%C*Eb)+sum(M3%*%M3*Eb3)/2, # T021c+201-Overlap
    "021c_210" = sum(C%*%C*Eb)+sum(M3%*%M3*(C3+t(C3)))/2, # T021c+T210-Overlap
    "021c_300" = sum(C%*%C*Eb)+sum(diag(M3%*%M3%*%M3))/6, # T021c+300-Overlap
    
    # TRIAD 030T
    "030t_003" = sum((C%*%C)*C)+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T030T in matrix A
    "030t_120c" = sum((C%*%C)*C)+sum(C3%*%C3*M3), # T030T+120c-Overlap
    "030t_120u" = sum((C%*%C)*C)+sum(C3%*%t(C3)*M3)/2, # T030T+120U-Overlap
    "030t_120d" = sum((C%*%C)*C)+sum(t(C)%*%C*M)/2, # T030T+T120D-Overlap
    "030t_210" = sum((C%*%C)*C)+sum(M3%*%M3*(C3+t(C3)))/2, # T030T+T210-Overlap
    "030t_300" = sum((C%*%C)*C)+sum(diag(M3%*%M3%*%M3))/6, # T030T+T300-Overlap
    
    # TRIAD 030C
    "030c_003" = sum(diag(C%*%C%*%C))/3+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T030C in matrix A
    "030c_120c" = sum(diag(C%*%C%*%C))/3+sum(C3%*%C3*M3), # T030C+120c-Overlap
    "030c_210" = sum(diag(C%*%C%*%C))/3+sum(M3%*%M3*(C3+t(C3)))/2, # T030C+T210-Overlap
    "030c_300" = sum(diag(C%*%C%*%C))/3+sum(diag(M3%*%M3%*%M3))/6, # T030C+T300-Overlap
    
    # TRIAD 111D
    "111d_003" = (sum(A%*%t(A)*Eb)-t201-t021U)/2+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T111d in matrix A
    "111d_210" = (sum(A%*%t(A)*Eb)-t201-t021U)/2+sum(M3%*%M3*(C3+t(C3)))/2, # T111d+T210-Overlap
    "111d_111d" = (sum(A%*%t(A)*Eb)-t201-t021U)/2+(sum(D%*%t(D)*Eb3)-t201b-t021Ub)/2, # T111d+111D-Overlap
    "111d_201" = (sum(A%*%t(A)*Eb)-t201-t021U)/2+sum(M3%*%M3*Eb3)/2, # T111d+T201-Overlap
    "111d_300" = (sum(A%*%t(A)*Eb)-t201-t021U)/2+sum(diag(M3%*%M3%*%M3))/6,
    
    # TRIAD 111U
    "111u_003" = (sum(t(A)%*%A*Eb)-t201-t021D)/2+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T111U in matrix A
    "111u_210" = (sum(t(A)%*%A*Eb)-t201-t021D)/2+sum(M3%*%M3*(C3+t(C3)))/2, # T111U+T210-Overlap
    "111u_111u" = (sum(t(A)%*%A*Eb)-t201-t021D)/2+(sum(t(D)%*%D*Eb3)-t201b-t021Db)/2, # T111U+111U-Overlap
    "111u_201" = (sum(t(A)%*%A*Eb)-t201-t021D)/2+sum(M3%*%M3*Eb3)/2, # T111U+T201-Overlap
    "111u_300" = (sum(t(A)%*%A*Eb)-t201-t021D)/2+sum(diag(M3%*%M3%*%M3))/6, # T111U+T300-Overlap
    
    # TRIAD 120U
    "120u_003" = sum(C%*%t(C)*M)/2+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T120u in matrix A
    "120u_120" = sum(C%*%t(C)*M)/2+sum(C3%*%t(C3)*M3)/2, # T120u+T120u-Overlap
    "120u_210" = sum(C%*%t(C)*M)/2+sum(M3%*%M3*(C3+t(C3)))/2, # T120u+T210-Overlap
    "120u_300" = sum(C%*%t(C)*M)/2+sum(diag(M3%*%M3%*%M3))/6, # T120u+T300-Overlap
    
    # TRIAD 120D
    "120d_003" = sum(t(C)%*%C*M)/2+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T120d in matrix A
    "120d_120d" = sum(t(C)%*%C*M)/2+sum(t(C3)%*%C3*M3)/2, # T120d+T120d-Overlap
    "120d_210" = sum(t(C)%*%C*M)/2+sum(M3%*%M3*(C3+t(C3)))/2, # T120d+T210-Overlap
    "120d_300" = sum(t(C)%*%C*M)/2+sum(diag(M3%*%M3%*%M3))/6, # T120d+T300-Overlap
    
    # TRIAD 120C
    "120c_003" = sum(C%*%C*M)+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T120c in matrix A
    "120c_120c" = sum(C%*%C*M)+sum(C3%*%C3*M3), # T120c+T120c-Overlap
    "120c_210" = sum(C%*%C*M)+sum(M3%*%M3*(C3+t(C3)))/2, # T120c+T210-Overlap
    "120c_300" = sum(C%*%C*M)+sum(diag(M3%*%M3%*%M3))/6, # T120c+T300-Overlap
    
    # TRIAD 201
    "201_003" = sum(M%*%M*Eb)/2+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T030C in matrix A
    "201_210" = sum(M%*%M*Eb)/2+sum(M3%*%M3*(C3+t(C3)))/2, # T030C+T210-Overlap
    "201_300" = sum(M%*%M*Eb)/2+sum(diag(M3%*%M3%*%M3))/6, # T030C+T300-Overlap
    
    # TRIAD 210
    "210_003" = sum(M%*%M*(C+t(C)))/2+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T030C in matrix A
    "210_210" = sum(M%*%M*(C+t(C)))/2+sum(M3%*%M3*(C3+t(C3)))/2, # T030C+T210-Overlap
    "210_300" = sum(M%*%M*(C+t(C)))/2+sum(diag(M3%*%M3%*%M3))/6, # T030C+T300-Overlap
    
    # TRIAD 300
    "300_003" = sum(diag(M%*%M%*%M))/6+sum(diag(Eb3%*%Eb3%*%Eb3))/6, # T300 in matrix A
    "300_300" = sum(diag(M%*%M%*%M))/6+sum(diag(M3%*%M3%*%M3))/6 # T300+T300-Overlap
    
  )
  res
}


### MIXED TRIAD CENSUS (HOLLWAY, LOMI, PALLOTTI AND STADTFELD, 2017)
mixedTriadCensus <- function (i, obsData, sims, period, groupName, varName, levls = 1:10, 
                              cumulative = FALSE, mode = "max") {
  
  if (length(varName) != 2) stop("mixedTriadCensus expects two varName parameters")
  varName1 <- varName[1]
  varName2 <- varName[2]
  
  m1 <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName1))
  m2 <- as.matrix(sparseMatrixExtraction(i, obsData, sims, period, groupName, varName = varName2))
  
  if ((dim(m1)[1] != dim(m1)[2]) | (dim(m1)[1] != dim(m2)[1])) stop("Error: The first element in varName must be one-mode, the second two-mode")
  
  cp <- function(m) (-m + 1)
  
  onemode.reciprocal <- m1 * t(m1)
  onemode.forward <- m1 * cp(t(m1))
  onemode.backward <- cp(m1) * t(m1)
  onemode.null <- cp(m1) * cp(t(m1))
  diag(onemode.forward) <- 0
  diag(onemode.backward) <- 0
  diag(onemode.null) <- 0
  
  bipartite.twopath <- m2 %*% t(m2)
  bipartite.null <- cp(m2) %*% cp(t(m2))
  bipartite.onestep1 <- m2 %*% cp(t(m2))
  bipartite.onestep2 <- cp(m2) %*% t(m2)
  diag(bipartite.twopath) <- 0
  diag(bipartite.null) <- 0
  diag(bipartite.onestep1) <- 0
  diag(bipartite.onestep2) <- 0
  
  res <- c("22" = sum(onemode.reciprocal * bipartite.twopath) / 2,
           "21" = sum(onemode.forward * bipartite.twopath) / 2 + sum(onemode.backward * bipartite.twopath) / 2,
           "20" = sum(onemode.null * bipartite.twopath) / 2,
           "12" = sum(onemode.reciprocal * bipartite.onestep1) / 2 + sum(onemode.reciprocal * bipartite.onestep2) / 2,
           "11D" = sum(onemode.forward * bipartite.onestep1) / 2 + sum(onemode.backward * bipartite.onestep2) / 2,
           "11U" = sum(onemode.forward * bipartite.onestep2) / 2 + sum(onemode.backward * bipartite.onestep1) / 2,
           "10" = sum(onemode.null * bipartite.onestep2) / 2 + sum(onemode.null * bipartite.onestep1) / 2,
           "02" = sum(onemode.reciprocal * bipartite.null) / 2,
           "01" = sum(onemode.forward * bipartite.null) / 2 + sum(onemode.backward * bipartite.null) / 2,
           "00" = sum(onemode.null * bipartite.null) / 2
  )
  
  dim1 <- dim(m2)[1]
  dim2 <- dim(m2)[2]
  nTriads <- dim2 * dim1 * (dim1 - 1) / 2
  if (sum(res) != nTriads) stop(paste("Error in calculation. More than",nTriads,"triads counted (", sum(res), ")"))
  
  res
}