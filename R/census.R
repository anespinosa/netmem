#' Dyad census
#'
#' @param G   A symmetric matrix object.
#' @param directed   Wheter the matrix is directed or not
#' 
#' @return This function return the counts of the dyadic census.
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' 
#' A <- matrix(c(0,1,0,1,0,0,1,0,0,1,0,0,0,1,0,0,0,0,0,0,0,
#'              1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,
#'              0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,
#'              1,1,0,0,0,0,1,0,0,1,0,0,0,1,1,0,0,0,0,0,0,
#'              0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,1,0,1,1,0,
#'              0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,1,1,1,
#'              0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'              0,0,1,0,1,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1,0,
#'              1,1,1,1,1,0,1,0,0,1,1,0,1,0,1,1,1,0,0,1,0,
#'              1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,
#'              0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
#'              0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,1,
#'              1,0,1,0,1,1,0,0,1,0,0,1,0,0,0,0,1,0,0,1,0,
#'              1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'              1,1,1,1,1,1,1,1,1,1,0,1,1,1,0,0,1,1,1,1,1,
#'              0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'              1,1,1,0,1,0,0,0,1,1,0,1,1,0,0,0,0,1,0,0,0,
#'              0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,
#'              0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,
#'              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), 
#'              ncol=21, byrow=TRUE)
#' 
#' dyadic_census(A)
#' 
#' @export

dyadic_census <- function(G, directed=TRUE){
  G <- as.matrix(G)
  g <- dim(G)[1]
  if(directed){
    c(
      "Mutual" = (1/2)*sum(diag(G%*%G)),
      "Asymmetrics" = sum(diag(G%*%t(G)))-sum(diag(G%*%G)),
      "Nulls" = ((g*(g-1))/2)-(sum(diag(G%*%t(G)))-sum(diag(G%*%G)))-((1/2)*sum(diag(G%*%G)))
    )
  }
  else{
    c("Mutual" = (1/2)*sum(diag(G%*%G)),
      "Nulls" = ((g*(g-1))/2)-(sum(diag(G%*%t(G)))-sum(diag(G%*%G)))-((1/2)*sum(diag(G%*%G))))
  }
}


#' Multiplex triad census for a directed and an undirected network.
#'
#' This function counts the different subgraphs of three nodes in a multiplex directed and undirected network.
#' 
#' @param A   A directed matrix object.
#' @param B   An undirected matrix object.
#' 
#' @return This function gives the counts of the mixed multiplex triad census for a directed and an undirected network. The contribution of each of the elementary form is 0.5 and in the multiplex triads sum 1.
#'
#' @references
#'
#' Espinosa-Rada, Alejandro; Bellotti, Elisa; Everett, Martin & Stadtfeld, Christoph (forthcoming). "Co-evolution of Scientific Networks: Multilevel Analysis of a National Discipline"
#' 
#' @author Alejandro Espinosa-Rada
#'
#'
#' @examples
#' 
#' # SOAR
#' A <- matrix(c(0,1,1,1,1,0,0,1,1,0,1,1,
#'               0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,1,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,1,1,1,0,0,0,1,1,0,1,1,
#'               0,0,0,1,0,0,0,0,1,0,0,0,
#'               0,0,0,1,0,1,0,0,1,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,1,1,1,1,0,0,1,0,0,1,1,
#'               0,1,0,0,0,1,0,0,0,0,0,0,
#'               0,1,0,0,1,0,0,0,1,0,0,1,
#'               0,0,0,0,0,0,0,0,0,0,0,0), 
#'             byrow=TRUE, ncol=12)
#' 
#' B <- matrix(c(0,0,0,0,1,0,0,0,1,0,0,0,
#'               0,0,0,0,0,0,0,0,1,0,0,1,
#'               0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,1,0,0,0,0,0,0,
#'               1,0,0,0,0,0,0,0,1,0,1,0,
#'               0,0,0,1,0,0,1,0,0,0,0,0,
#'               0,0,0,0,0,1,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,
#'               1,1,0,0,1,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,1,0,0,0,0,0,0,0,
#'               0,1,0,0,0,0,0,0,0,0,0,0), 
#'             byrow = TRUE, ncol=12)
#' 
#' MixedLayerTriadCensus(A, B)
#' @export
#' 

MixedLayerTriadCensus <- function(A, B){
  warning("Experimental version, please use with caution!")
  
  if(!all(A<=1))warning(paste("Measure only implemented for binary networks,", "the `first` network","would be binarized for the triadic census"))
  A <- as.matrix(A)
  if(!dim(A)[1]==dim(A)[2])stop("Matrix should be square")
  A <- ifelse(A>0, 1, 0) # Binarize
  
  if(!all(B<=1))warning(paste("Measure only implemented for binary networks,", "the `second` network","would be binarized for the triadic census")) #deparse(quote(B))
  B <- as.matrix(B)
  if(!dim(B)[1]==dim(B)[2])stop("Matrix should be square")
  B <- ifelse(B>0, 1, 0) # Binarize
  
  if(!all(B[lower.tri(B)] == t(B)[lower.tri(B)]))warning(paste("Measure only implemented for undirected networks", "in the `second` network,", "the measure would use the underlying graph"))
  B[lower.tri(B)] = t(B)[lower.tri(B)] # Symmetrize
  
  # MATRIX A
  E <- ifelse((A+t(A))>0, 1, 0) # arcs to edges
  Eb <- ifelse(E==0, 1, 0)
  diag(Eb) <- 0
  M <- ifelse((A+t(A))>1, 1, 0) # mutual edges only
  C <- A-M # asymmetric arcs only
  
  t201=sum(M%*%M*Eb) # null dyad in a 201 triad
  t021D=sum(t(C)%*%C*Eb) # null dyad in an 021D triad  
  t021U=sum(C%*%t(C)*Eb) # null dyad in an 021U triad
  
  # MATRIX B
  E2 <- ifelse((B+t(B))>0, 1, 0) # arcs to edges
  Eb2 <- ifelse(E2==0, 1, 0)
  diag(Eb2) <- 0
  M2 <- ifelse((B+t(B))>1, 1, 0) # mutual edges only
  C2 <- B-M # asymmetric arcs only
  
  t201b=sum(M%*%M*Eb) # null dyad in a 201 triad
  t021Db=sum(t(C2)%*%C2*Eb2) # null dyad in an 021D triad  
  t021Ub=sum(C2%*%t(C2)*Eb2) # null dyad in an 021U triad
  
  # MATRIX A+B
  D <- (A+B) 
  D <- ifelse(D>=1, 1, 0)
  E3 <- ifelse((D+t(D))>0, 1, 0) # arcs to edges
  Eb3 <- ifelse(E3==0, 1, 0)
  diag(Eb3) <- 0
  M3 <- ifelse((D+t(D))>1, 1, 0) # mutual edges only
  C3 <- D-M3 # asymmetric arcs only
  
  t201d=sum(M3%*%M3*Eb3) # null dyad in a 201 triad
  t021Dd=sum(t(C3)%*%C3*Eb3) # null dyad in an 021D triad  
  t021Ud=sum(C3%*%t(C3)*Eb3) # null dyad in an 021U triad
  
  res <- c(
    
    #----#
    # TRIAD 003
    "003_003" = sum(diag(Eb3%*%Eb3%*%Eb3))/6, 
    "003_102" = sum(diag(Eb%*%Eb%*%Eb))/6+sum((Eb2%*%Eb2*M2))/2,
    "003_201" = sum(diag(Eb%*%Eb%*%Eb))/6+sum(M2%*%M2*Eb2)/2, 
    "003_300" = sum(diag(Eb%*%Eb%*%Eb))/6+sum(diag(M2%*%M2%*%M2))/6, 
    
    # TRIAD 012
    "012_003" = sum((Eb%*%Eb)*(C+t(C)))/2+sum(diag(Eb2%*%Eb2%*%Eb2))/6,
    "012_102a" = sum((Eb%*%Eb)*(C+t(C)))/2+sum((Eb3%*%Eb3*M3))/2, #Overlap
    "012_102b" = sum((Eb%*%Eb)*(C+t(C)))/2+(sum(t(D)%*%D*Eb3)-t201d-t021Dd)/2, # T012+111U-Overlap
    "012_102c" = sum((Eb%*%Eb)*(C+t(C)))/2+(sum(D%*%t(D)*Eb3)-t201d-t021Ud)/2, # T012+111D-Overlap
    "012_201b" = sum((Eb%*%Eb)*(C+t(C)))/2+sum(M3%*%M3*(C3+t(C3)))/2, # T012+T210-Overlap
    "012_201ac" = sum((Eb%*%Eb)*(C+t(C)))/2+sum(M3%*%M3*Eb3)/2, # T012+T201-Overlap
    "012_300" = sum((Eb%*%Eb)*(C+t(C)))/2+sum(diag(M3%*%M3%*%M3))/6,
    
    # TRIAD 021U
    "021u_003" = sum(C%*%t(C)*Eb)/2+sum(diag(Eb2%*%Eb2%*%Eb2))/6, # T021u in matrix A
    "021u_102ac" = sum(C%*%t(C)*Eb)/2+(sum(D%*%t(D)*Eb3)-t201d-t021Ud)/2, # T021u+T021U-Overlap
    "021u_102b" = sum(C%*%t(C)*Eb)/2+sum(C3%*%t(C3)*M3)/2, # T021u+T120U-Overlap
    "021u_201ab" = sum(C%*%t(C)*Eb)/2+sum(M3%*%M3*(C3+t(C3)))/2,
    "021u_201c" = sum(C%*%t(C)*Eb)/2+sum(M3%*%M3*Eb3)/2, # T021d+210-Overlap
    "021u_300" = sum(C%*%t(C)*Eb)/2+sum(diag(M3%*%M3%*%M3))/6, # T021c+300-Overlap
    
    # TRIAD 021D
    "021d_003" = sum(t(C)%*%C*Eb)/2+sum(diag(Eb2%*%Eb2%*%Eb2))/6,
    "021d_102ac" = sum(t(C)%*%C*Eb)/2+(sum(t(D)%*%D*Eb3)-t201d-t021Dd)/2, # Overlap
    "021d_120b" = sum(t(C)%*%C*Eb)/2+sum(t(C3)%*%C3*M3)/2, # Overlap
    "021d_201ab" = sum(t(C)%*%C*Eb)/2+sum(M3%*%M3*(C3+t(C3)))/2, # Overlap
    "021d_201c" = sum(t(C)%*%C*Eb)/2+sum(M3%*%M3*Eb3)/2, # Overlap
    "021d_300" = sum(t(C)%*%C*Eb)/2+sum(diag(M2%*%M2%*%M2))/6,
    
    # TRIAD 102
    #"102_003" = sum((Eb%*%Eb*M))/2+sum(diag(Eb2%*%Eb2%*%Eb2))/6,
    "102_102a" = sum((Eb%*%Eb*M))/2+sum((Eb3%*%Eb3*M3))/2, # Overlap
    "102_102bc_201ac" = sum((Eb%*%Eb*M))/2+sum(M3%*%M3*Eb3)/2, # Overlap
    "102_300" = sum((Eb%*%Eb*M))/2+sum(diag(M2%*%M2%*%M2))/6,
    
    # TRIAD 021C
    "021c_003" = sum(C%*%C*Eb)+sum(diag(Eb2%*%Eb2%*%Eb2))/6,
    "021c_102a" = sum(C%*%C*Eb)+(sum(t(D)%*%D*Eb3)-t201d-t021Dd)/2, # Overlap
    "021c_103b" = sum(C%*%C*Eb)+sum(C3%*%C3*M3), # Overlap
    "021c_102c" = sum(C%*%C*Eb)+(sum(D%*%t(D)*Eb3)-t201d-t021Ud)/2, # Overlap
    "021c_210ab" = sum(C%*%C*Eb)+sum(M3%*%M3*(C3+t(C3)))/2, # Overlap
    "021c_201c" = sum(C%*%C*Eb)+sum(M3%*%M3*Eb3)/2, # Overlap
    "021c_300" = sum(C%*%C*Eb)+sum(diag(M3%*%M3%*%M3))/6, # Overlap
    
    # TRIAD 030T
    "030t_003" = sum((C%*%C)*C)+sum(diag(Eb2%*%Eb2%*%Eb2))/6, 
    "030t_102ab" = sum((C%*%C)*C)+sum(C3%*%C3*M3), # Overlap
    "030t_102b" = sum((C%*%C)*C)+sum(C3%*%t(C3)*M3)/2, # Overlap
    "030t_102c" = sum((C%*%C)*C)+sum(t(C3)%*%C3*M3)/2, # Overlap
    "030t_210ab" = sum((C%*%C)*C)+sum(M3%*%M3*(C3+t(C3)))/2, # Overlap
    "030t_201c_300" = sum((C%*%C)*C)+sum(diag(M3%*%M3%*%M3))/6, # -Overlap
    
    # TRIAD 030C
    "030c_003" = sum(diag(C%*%C%*%C))/3+sum(diag(Eb2%*%Eb2%*%Eb2))/6, 
    "030c_102abc" = sum(diag(C%*%C%*%C))/3+sum(C3%*%C3*M3), # Overlap
    "030c_201abc" = sum(diag(C%*%C%*%C))/3+sum(M3%*%M3*(C3+t(C3)))/2, # Overlap
    "030c_300" = sum(diag(C%*%C%*%C))/3+sum(diag(M3%*%M3%*%M3))/6, # Overlap
    
    # TRIAD 111D
    "111d_003" = (sum(A%*%t(A)*Eb)-t201-t021U)/2+sum(diag(Eb2%*%Eb2%*%Eb2))/6, 
    "111d_102a_201a" = (sum(A%*%t(A)*Eb)-t201-t021U)/2+sum(M3%*%M3*(C3+t(C3)))/2, #Overlap
    "111d_102b" = (sum(D%*%t(D)*Eb3)-t201d-t021Ud)/2, # Overlap
    "111d_102c_201b" = (sum(A%*%t(A)*Eb)-t201-t021U)/2+sum(M3%*%M3*Eb3)/2, # Overlap
    "111d_201c_300" = (sum(A%*%t(A)*Eb)-t201-t021U)/2+sum(diag(M3%*%M3%*%M3))/6,
    
    # TRIAD 111U
    "111u_003" = (sum(t(A)%*%A*Eb)-t201-t021D)/2+sum(diag(Eb2%*%Eb2%*%Eb2))/6,
    "111u_102a_201a" = (sum(t(A)%*%A*Eb)-t201-t021D)/2+sum(M3%*%M3*(C3+t(C3)))/2, #Overlap
    "111u_102bc_201b" = (sum(t(A)%*%A*Eb)-t201-t021D)/2+sum(M3%*%M3*Eb3)/2, # Overlap
    "111u_201c_300" = (sum(t(A)%*%A*Eb)-t201-t021D)/2+sum(diag(M3%*%M3%*%M3))/6, # Overlap
    
    # TRIAD 120U
    #"120u_003" = sum(C%*%t(C)*M)/2+sum(diag(Eb2%*%Eb2%*%Eb2))/6, 
    "120u_102b" = sum(C%*%t(C)*M)/2+sum(C3%*%t(C3)*M3)/2, # Overlap # CHECK!
    "120u_102ab_201ab" = sum(C%*%t(C)*M)/2+sum(M3%*%M3*(C3+t(C3)))/2, # Overlap
    "120u_201c_300" = sum(C%*%t(C)*M)/2+sum(diag(M3%*%M3%*%M3))/6, # Overlap
    
    # TRIAD 120D
    #"120d_003" = sum(t(C)%*%C*M)/2+sum(diag(Eb2%*%Eb2%*%Eb2))/6, # CHECK!
    "120d_120b" = sum(t(C)%*%C*M)/2+sum(t(C3)%*%C3*M3)/2, # Overlap # CHECK!
    "120d_102ab_201ab" = sum(t(C)%*%C*M)/2+sum(M3%*%M3*(C3+t(C3)))/2, # Overlap
    "120d_201c_300" = sum(t(C)%*%C*M)/2+sum(diag(M3%*%M3%*%M3))/6, # Overlap
    
    # TRIAD 201
    "201_003" = sum(M%*%M*Eb)/2+sum(diag(Eb2%*%Eb2%*%Eb2))/6, # T030C in matrix A
    "201_102ac_201ab" = sum(M%*%M*Eb)/2+sum(M3%*%M3*Eb3)/2, # T030C+T210-Overlap
    "201_102c_201bc_300" = sum(M%*%M*Eb)/2+sum(diag(M3%*%M3%*%M3))/6, # T030C+T300-Overlap
    
    # TRIAD 120C
    "120c_003" = sum(C%*%C*M)+sum(diag(Eb2%*%Eb2%*%Eb2))/6, # T120c in matrix A
    "120c_120c" = sum(C%*%C*M)+sum(C3%*%C3*M3), # T120c+T120c-Overlap
    "120c_210" = sum(C%*%C*M)+sum(M3%*%M3*(C3+t(C3)))/2, # T120c+T210-Overlap
    "120c_300" = sum(C%*%C*M)+sum(diag(M3%*%M3%*%M3))/6, # T120c+T300-Overlap
    
    # TRIAD 210
    #"210_003" = sum(M%*%M*(C+t(C)))/2+sum(diag(Eb2%*%Eb2%*%Eb2))/6, # CHECK!
    "210_210" = sum(M%*%M*(C+t(C)))/2+sum(M3%*%M3*(C3+t(C3)))/2,
    "210_300" = sum(M%*%M*(C+t(C)))/2+sum(diag(M3%*%M3%*%M3))/6, # T030C+T300-Overlap
    
    # TRIAD 300
    #"300_003" = sum(diag(M%*%M%*%M))/6+sum(diag(Eb2%*%Eb2%*%Eb2))/6,  # CHECK!
    "300_300" = sum(diag(M%*%M%*%M))/6+sum(diag(M3%*%M3%*%M3))/6 # T300+T300-Overlap
    
  )
  return(res/2)
}
