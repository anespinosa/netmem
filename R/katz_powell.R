### Katz and Powell (1955)
pkp <- function(G, fixed=FALSE, d=NULL, dichotomic=TRUE)
{
  G <- as.matrix(G)
  g <- dim(G)[1]
  if(dichotomic){
    G <- ifelse(G>=1, 1, 0)
  }
  else warning("This measure is not well specified for weighted network")
  M <- (1/2)*sum(diag(G%*%G))
  if(fixed){
    if(is.null(d)) stop("For fixed design `d` should be specified")
    (((2*(g-1))*M)-(g*(d^2)))/((g*d)*(g-1-d))
  }
  else{
    L <- sum(diag(G%*%t(G)))-sum(diag(G%*%G))
    L2 <- sum(rowSums(G)^2)
    ((2*((g-1)^2)*M)-(L^2)+L2)/((L*(g-1)^2)-(L^2)+L2)
  }
}
pkp(SC)