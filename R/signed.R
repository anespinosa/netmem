#' Structural balance
#'
#' @param A   A signed symmetric matrix (i.e., with ties that are either -1, 0 or 1)
#' @param B   A signed symmetric matrix considered as the negative ties (i.e., with ties that are either -1, 0 or 1)
#' @param score   Whether to return the \code{triangle} (default) or \code{walk} balance score (Aref and Wilson, 2017)
#'
#' @return This function return the structural balance (Heider, 1940; Cartwright and Harary, 1956).
#' When \code{B} is used, matrix A is considered the negative matrix and \code{A} the positive matrix.
#'
#' @references
#'
#' Aref, Samin and Wilson, Mark C. (2017). Measuring partial balance in signed networks. Journal of Complex Networks, 6(4): 566-595.
#'
#' Cartwright, Dorwin, and Harary, Frank (1956). Structural balance: a generalization of Heider's theory. Psychological review, 63(5), 277.
#'
#' Heider, Fritz (1946). Attitudes and Cognitive Organization. The Journal of Psychology, 21: 107–112
#'
#' @importFrom stats aggregate
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, -1, -1, 0,
#'   -1, 0, 1, 0,
#'   -1, 1, 0, 0,
#'   0, 0, 0, 0
#' ), byrow = TRUE, ncol = 4)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#' struc_balance(A)
#' @export

# TODO: Expand measure

struc_balance <- function(A, B = NULL, score = c("triangle", "walk")) {
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (is.null(rownames(A))) stop("No label assigned to the rows of the matrix")
  if (is.null(colnames(A))) stop("No label assigned to the columns of the matrix")
  if (all(rownames(A) != colnames(A))) stop("The names of rows and columns does not match")
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(abs(A > 1), na.rm = TRUE)) warning("The matrix should be binary")
  if (!all(A[lower.tri(A)] == t(A)[lower.tri(A)], na.rm = TRUE)) warning("The network is directed. The underlying graph is used")
  A[lower.tri(A)] <- t(A)[lower.tri(A)] # Symmetrize

  if (!is.null(B)) {
    if (any(is.na(B) == TRUE)) {
      B <- ifelse(is.na(B), 0, B)
    }
    if (is.null(rownames(B))) stop("No label assigned to the rows of the matrix")
    if (is.null(colnames(B))) stop("No label assigned to the columns of the matrix")
    if (all(rownames(B) != colnames(B))) stop("The names of rows and columns does not match")
    if (nrow(B) != ncol(B)) stop("Matrix should be square")

    if (any(abs(B > 1), na.rm = TRUE)) warning("The matrix should be binary")
    if (any((abs(A) + abs(B)) > 1)) {
      message("Some ties are positive and negative at the same time, which might distort the number of cliques in the matrix")
    }

    if (!all(B[lower.tri(B)] == t(B)[lower.tri(B)], na.rm = TRUE)) warning("The network is directed. The underlying graph is used")
    B[lower.tri(B)] <- t(B)[lower.tri(B)] # Symmetrize

    if (!all(dim(A) == dim(B))) stop("Non-conformable arrays")

    B[which(B >= 1)] <- -1
    Csign <- as.matrix(A + B)
  } else {
    Csign <- as.matrix(A)
  }

  C <- ifelse(abs(Csign) >= 1, 1, 0)

  triads <- do.call(rbind, clique_table(C, list_cliques = TRUE)$neighbours)
  edgelist <- matrix_to_edgelist(C, valued = FALSE, digraph = TRUE, loops = FALSE)
  sign <- c(Csign)[c(Csign) != 0]
  edgesign <- cbind(edgelist, sign)
  colnames(edgesign) <- c("from", "to", "sign")
  edgesign

  test1 <- list()
  test2 <- list()
  test3 <- list()
  for (i in 1:nrow(triads)) {
    test1[[i]] <- edgesign[, 3][which(paste(triads[, 1:2][i, ],
      collapse = ""
    ) == paste(
      edgesign[, 1], edgesign[, 2],
      sep = ""
    ))]
    test2[[i]] <- edgesign[, 3][which(paste(triads[, 2:3][i, ],
      collapse = ""
    ) == paste(
      edgesign[, 1], edgesign[, 2],
      sep = ""
    ))]
    test3[[i]] <- edgesign[, 3][which(paste(triads[, c(1, 3)][i, ],
      collapse = ""
    ) == paste(
      edgesign[, 1], edgesign[, 2],
      sep = ""
    ))]
  }
  triads <- cbind(
    do.call(rbind, test1),
    do.call(rbind, test2),
    do.call(rbind, test3)
  )
  triads <- t(apply(triads, 1, sort))
  triads <- as.data.frame(triads)
  res <- stats::aggregate(
    list(count = rep(1, nrow(triads))),
    triads, length
  )
  colnames(res) <- c("sign1", "sign2", "sign3", "number")
  for (i in 1:nrow(res)) {
    res$balance[i] <- length(grep("-", res[i, ]))
  }
  res$balance[res$balance == 0] <- "+++"
  res$balance[res$balance == 1] <- "-++"
  res$balance[res$balance == 2] <- "--+"
  res$balance[res$balance == 3] <- "---"

  score <- switch(method_used(score),
    "triangle" = 1,
    "walk" = 2
  )

  if (score == 1) {
    a <- res$number[res$balance == "+++"]
    if (length(a) == 0) {
      a <- 0
    }
    b <- res$number[res$balance == "--+"]
    if (length(b) == 0) {
      b <- 0
    }
    score <- (a + b) / sum(res$number)
    return(list(table = res, balance_score = score))
  } else {
    if (score == 2) {
      eigen_sign <- eigen(Csign)$values
      eigen_total <- eigen(abs(Csign))$values
      score <- sum(exp(eigen_sign)) / sum(exp(eigen_total))
      return(list(table = res, balance_score = score))
    }
  }
}

method_used <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  match.arg(arg = arg, choices = choices, several.ok = several.ok)
}



#' Positive-negative centrality
#'
#' @param A   A signed symmetric matrix (i.e., with ties that are either -1, 0 or 1)
#' @param select   Whether to consider the direction of the outgoing ties. Considering \code{all} (default), \code{in} or \code{out} ties.
#'
#' @return This function return the positive-negative centrality index for signed networks (Everett and Borgatti).
#'
#' @references
#'
#' Everett, Martin and Borgatti, Stephen (2014). Networks containing negative ties. Social Networks, 38, 111-120. \url{http://dx.doi.org/10.1016/j.socnet.2014.03.005}
#'
#' @source Adapted from David Schoch 'signnet'
#'
#' @examples
#'
#' A <- matrix(
#'   c(
#'     0, 1, -1, -1, -1, -1, 0, 0, 0, 0, 0, -1, 0, 0, 1, 1,
#'     1, 0, -1, 0, -1, -1, 0, 0, -1, -1, 0, 0, 0, 0, 1, 1,
#'     -1, -1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
#'     -1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
#'     -1, -1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, -1, -1,
#'     -1, -1, 1, 0, 0, 0, 1, 1, -1, 0, 1, 1, -1, 0, 0, -1,
#'     0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0,
#'     0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 1, 0, -1, 0, 0,
#'     0, -1, 0, 0, 1, -1, 0, 0, 0, 1, -1, 0, 1, 0, -1, 0,
#'     0, -1, 0, 0, 0, 0, 0, 0, 1, 0, -1, 0, 1, 0, -1, 0,
#'     0, 0, 0, 0, 0, 1, 1, 1, -1, -1, 0, 1, -1, 0, -1, -1,
#'     -1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, -1, -1, -1,
#'     0, 0, 0, 0, 0, -1, 1, 0, 1, 1, -1, 0, 0, 1, -1, -1,
#'     0, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0, -1, 1, 0, 0, -1,
#'     1, 1, 0, 0, -1, 0, 0, 0, -1, -1, -1, -1, -1, 0, 0, 1,
#'     1, 1, 0, 0, -1, -1, 0, 0, 0, 0, -1, -1, -1, -1, 1, 0
#'   ),
#'   ncol = 16, nrow = 16, byrow = TRUE
#' )
#' label <- c(
#'   "Gavev", "Kotun", "Ove", "Alika", "Nagam", "Gahuk", "Masil", "Ukudz",
#'   "Notoh", "Kohik", "Geham", "Asaro", "Uheto", "Seuve", "Nagad", "Gama"
#' )
#' rownames(A) <- label
#' colnames(A) <- rownames(A)
#' posneg_index(A, select = c("all"))
#' @export

posneg_index <- function(A, select = c("all", "in", "out")) {
  if (any(is.na(A))) {
    A <- ifelse(is.na(A), 0, A)
  }
  if (any(abs(A > 1), na.rm = TRUE)) warning("The matrix should be binary")
  if (!any(A < 0)) warning("No negative ties")
  if (!any(A > 0)) warning("No positive ties")

  select <- switch(node_direction(select),
    "out" = 1,
    "in" = 2,
    "all" = 3
  )

  pos <- ifelse(A == 1, 1, 0)
  neg <- ifelse(A == -1, 1, 0)

  I <- diag(1, ncol(A))
  A <- pos - 2 * neg

  if (select == 1) {
    # all
    a <- solve(I - 1 / (2 * ncol(A) - 2) * A)
    return(rowSums(a))
  }

  if (select == 2) {
    # in
    a <- solve(I - 1 / (4 * (ncol(A) - 1)^2) * t(A) %*% A) %*% (I + 1 / (2 * ncol(A) - 2) * t(A))
    return(rowSums(a))
  }

  if (select == 3) {
    # out
    a <- solve(I - 1 / (4 * (ncol(A) - 1)^2) * A %*% t(A)) %*% (I + 1 / (2 * ncol(A) - 2) * A)
    return(rowSums(a))
  }
}

# TODO: structural equivalence (Lorrain and White, 1971), automorphic equivalence and regular equivalence (White and Reitz, 1983; Everett and Borgatti, 1995)
# TODO: Bonacich and Lloyd (2004) eigenvector centrality
# TODO: http://dx.doi.org/10.1016/j.socnet.2013.04.007
