#' Transitivity
#'
#' This measure is sometimes called clustering coefficient.
#'
#' @param A   A matrix
#' @param method   Whether to calculate the \code{global} transitivity ratio, the \code{mean} transitivity or the \code{local} transitivity.
#' @param select    Whether to consider \code{all}, \code{in} or \code{out} ties for the local transitivity.
#'
#' @return Return a transitivity measure
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(c(
#'   0, 1, 0, 1, 0,
#'   1, 0, 1, 1, 0,
#'   0, 1, 0, 0, 0,
#'   1, 1, 0, 0, 1,
#'   0, 0, 0, 1, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(A) <- letters[1:ncol(A)]
#' colnames(A) <- rownames(A)
#'
#' trans_coef(A, method = "local")
#' @export

# TODO: BUG! trans_coef(A, method = "mean") unlist(local_trans(A))

trans_coef <- function(A, method = c("global", "mean", "local"),
                       select = c("all", "in", "out")) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }

  method <- switch(method_option(method),
    "global" = 1,
    "mean" = 2,
    "local" = 3
  )

  if (method == 1) {
    B <- A %*% A
    diag(B) <- 0
    return(sum(diag(A %*% A %*% A)) / sum(B))
  }

  if (method == 2 | method == 3) {
    select <- switch(node_direction(select),
      "out" = 1,
      "in" = 2,
      "all" = 3
    )
    local_trans <- list()
    for (i in 1:ncol(A)) {
      subA <- ego_net(A, ego = rownames(A)[i], select = "all")

      if (all(dim(subA) == 1)) {
        total_pairs <- 0
        pairs <- 0
      } else {
        pairs <- sum(subA) / 2
        total_pairs <- 1 / 2 * ncol(subA) * (ncol(subA) - 1)
      }
      local_trans[[i]] <- pairs / total_pairs
      names(local_trans)[i] <- rownames(A)[i]
    }
  }

  if (method == 2) {
    return(mean = mean(unlist(local_trans(A)), na.rm = TRUE))
  }

  if (method == 3) {
    return(local_trans)
  }
}


method_option <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  match.arg(arg = arg, choices = choices, several.ok = several.ok)
}


#' Components
#'
#' This function assigns an id to the components that each of the nodes of the matrix belongs
#'
#' @param A   A matrix
#'
#' @return A vector assigning an id the components that each of the nodes of the matrix belongs
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(c(
#'   0, 1, 1, 0, 0,
#'   1, 0, 1, 0, 0,
#'   1, 1, 0, 0, 0,
#'   0, 0, 0, 0, 1,
#'   0, 0, 0, 1, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(A) <- letters[1:ncol(A)]
#' colnames(A) <- rownames(A)
#' components_id(A)
#' @export

# TODO: add direction (weak or strong connection)
# TODO: correct for two-mode networks
components_id <- function(A) {
  A <- as.matrix(A)

  # if(bipartite){
  #   A <- A%*%t(A)
  # }

  A[A > 0] <- 1
  dist <- bfs_ugraph(A)
  dist[dist != Inf] <- 1
  reachable <- list()
  for (i in 1:ncol(dist)) {
    reachable[[i]] <- which(dist[i, ] == 1)
  }
  components <- as.numeric(as.factor(as.character(paste(reachable))))
  t <- table(components)
  return(list(components = components, size = t))
}
