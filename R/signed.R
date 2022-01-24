#' Structural balance
#'
#' @param A   A signed matrix (i.e., with ties that are either -1, 0 or 1)
#'
#' @return This function return the structural balance (Heider, 1940; Cartwright and Harary, 1956)
#'
#' @references 
#' 
#' Cartwright, Dorwin, and Harary, Frank (1956). Structural balance: a generalization of Heider's theory. Psychological review, 63(5), 277.
#' 
#' Heider, Fritz (1946). "Attitudes and Cognitive Organization". The Journal of Psychology. 21: 107–112
#'
#' @importFrom stats aggregate
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' B <- matrix(c(0,-1,-1,0,
#'               -1,0,1,0,
#'               -1,1,0,0,
#'               0,0,0,0), byrow = TRUE, ncol = 4)
#' rownames(B) <- letters[1:nrow(B)]
#' colnames(B) <- rownames(B)
#' struc_balance(B)
#'
#' @export


struc_balance <- function(A){
  
  if(any(is.na(A) == TRUE)){
    A <- ifelse(is.na(A), 0, A)
  }
  if(is.null(rownames(A)))stop("No label assigned to the rows of the matrix")
  if(is.null(colnames(A)))stop("No label assigned to the columns of the matrix")
  if(all(rownames(A) != colnames(A)))stop("The names of rows and columns does not match")
  if(nrow(A)!=ncol(A))stop("Matrix should be square")
  if(any(abs(A>1), na.rm = TRUE))warning("The matrix should be binary")
  if(!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE))warning("The network is directed. The underlying graph is used")
  A[lower.tri(A)] = t(A)[lower.tri(A)] # Symmetrize
  
  Csign <- as.matrix(A)
  C <- ifelse(abs(Csign)>=1, 1, 0)
  
  triads <- do.call(rbind, clique_table(C, list_cliques = TRUE)$neighbours)
  edgelist <- matrix_to_edgelist(C)
  sign <- c(Csign)[c(Csign)!=0]
  edgesign <- cbind(edgelist, sign)
  colnames(edgesign) <- c('from', 'to', 'sign')
  edgesign
  
  test1 <- list()
  test2 <- list()
  test3 <- list()
  for(i in 1:nrow(triads)){
    test1[[i]] <- edgesign[,3][which(paste(triads[,1:2][i,], 
                                           collapse = '') == paste(
                                             edgesign[,1], edgesign[,2], 
                                             sep = ''))]
    test2[[i]] <- edgesign[,3][which(paste(triads[,2:3][i,], 
                                           collapse = '') == paste(
                                             edgesign[,1], edgesign[,2], 
                                             sep = ''))]
    test3[[i]] <- edgesign[,3][which(paste(triads[,c(1,3)][i,], 
                                           collapse = '') == paste(
                                             edgesign[,1], edgesign[,2], 
                                             sep = ''))]
    
  }
  triads <- cbind(do.call(rbind, test1),
                  do.call(rbind, test2),
                  do.call(rbind, test3))
  triads <- t(apply(triads, 1, sort))
  triads <- as.data.frame(triads)
  res <- stats::aggregate(list(count = rep(1, nrow(triads))), 
                          triads, length)
  colnames(res) <- c('sign1', 'sign1', 'sign1', 'number')
  return(res)
}