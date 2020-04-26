
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