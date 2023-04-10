#' Reciprocity
#'
#' This measure calculated the reciprocity of an asymmetric matrix (directed graph).
#'
#' @param A   A matrix
#' @param diag   Whether to consider the diagonal of the matrix
#' @param method   Whether to use \code{total_ratio}, \code{ratio_nonnull} or \code{global} reciprocity
#'
#' @return Return a reciprocity coefficient
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#' A <- matrix(c(0,1,1,0,
#' 1,0,1,0,
#' 0,0,0,0,
#' 1,0,0,0), byrow = TRUE, ncol = 4)
#' recip_coef(A)
#' @export

recip_coef <- function(A, diag = NULL, method = c("total_ratio", "ratio_nonnull", "global")) {
  if (all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) {
    message("Matrix is symmetric (network is undirected)")
  }

  if (is.null) {
    diag(A) <- 0
  }

  method <- switch(method_option(method),
    "total_ratio" = 1,
    "ratio_nonnull" = 2,
    "global" = 3
  )
  dyad <- dyadic_census(A)
  if (method == 1) {
    # proportion of dyads that are symmetric
    return((dyad[1] + dyad[3]) / sum(unlist(dyad)))
  }
  if (method == 2) {
    # reciprocity, ignoring the null dyads
    return((dyad[1]) / (dyad[1] + dyad[2]))
  }
  if (method == 3) {
    # global reciprocity
    return(sum(A * t(A)) / sum(A))
  }
}

#' Transitivity
#'
#' This measure is sometimes called clustering coefficient.
#'
#' @param A   A matrix
#' @param method   Whether to calculate the \code{weakcensus}, \code{global} transitivity ratio, the \code{mean} transitivity or the \code{local} transitivity.
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

# TODO: Improve documentation, add Barrat's measure, rank condition, correlation option of Dekker, strong census, and expand if necessary.

trans_coef <- function(A, method = c("weakcensus", "global", "mean", "local"),
                       select = c("all", "in", "out")) {
  A <- as.matrix(A)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }

  method <- switch(method_option(method),
    "weakcensus" = 1,
    "global" = 2,
    "mean" = 3,
    "local" = 4
  )

  if (method == 1) {
    path2_A <- (A %*% A)
    diag(path2_A) <- 0
    return(sum(A * path2_A, na.rm = TRUE) / sum(path2_A, na.rm = TRUE))
  }

  if (method == 2) {
    if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) {
      message("Matrix is asymmetric (network is directed), the underlying graph is used")
    }
    A <- A + t(A) # Symmetrize
    A[A > 0] <- 1

    B <- A %*% A
    diag(B) <- 0
    return(sum(diag(A %*% A %*% A)) / sum(B))
  }

  if (method == 3 | method == 4) {
    if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) {
      message("Matrix is asymmetric (network is directed), the underlying graph is used")
    }
    A <- A + t(A) # Symmetrize
    A[A > 0] <- 1

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

  if (method == 3) {
    return(mean = mean(unlist(local_trans(A)), na.rm = TRUE))
  }

  if (method == 4) {
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

#' Transitivity matrix
#'
#' This function assigns a one in the elements of the matrix if a group of actors are part of a transitivity structure (030T label considering the MAN triad census)
#'
#' @param A   A matrix
#' @param loops  Whether to expect nonzero elements in the diagonal of the matrix
#'
#' @return A vector assigning an id the components that each of the nodes of the matrix belongs
#'
#' @references
#'
#' Davis, J.A. and Leinhardt, S. (1972). “The Structure of Positive Interpersonal Relations in Small Groups.” In J. Berger (Ed.), Sociological Theories in Progress, Vol. 2, 218-251. Boston: Houghton Mifflin.
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A <- matrix(
#'   c(
#'     0, 1, 1, 0, 0, 0,
#'     0, 0, 1, 0, 0, 0,
#'     0, 0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 0, 0,
#'     0, 0, 1, 1, 0, 0,
#'     0, 0, 0, 0, 0, 0
#'   ),
#'   byrow = TRUE, ncol = 6
#' )
#' rownames(A) <- letters[1:NROW(A)]
#' colnames(A) <- rownames(A)
#' trans_matrix(A, loops = TRUE)
#'
#' @export

trans_matrix <- function(A, loops = FALSE) {
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }

  if (is.null(rownames(A))) stop("No label assigned to the columns of the matrix")
  if (is.null(colnames(A))) stop("No label assigned to the columns of the matrix")

  B <- matrix(0, ncol = NCOL(A), NROW(A))
  rownames(B) <- rownames(A)
  colnames(B) <- colnames(A)
  actors <- list()
  for (i in 1:NROW(A)) {
    C030T <- (A %*% A) * A
    actors[[i]] <- names(which(C030T[i, ] == 1))
    if (length(actors[[i]]) >= 1) {
      temp <- colnames(ego_net(A, ego = actors[[i]], select = c("in")))
      temp
      temp2 <- names(which(rowSums(A[temp, temp]) >= 1))
      names_030T <- c(temp2, names(which(A[temp2, ] >= 1)))
      names_030T <- sort(names_030T)
      B[names_030T, names_030T] <- 1
    } else {
      next
    }
  }

  if (!loops) {
    diag(B) <- 0
  }
  return(B)
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
