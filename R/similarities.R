#' Citation networks
#'
#' Matrix transformation from incident matrices to a citation, fractional counting for co-citation or fractional counting for bibliographic coupling
#'
#' @param A1   From incident matrix of paper and author.
#' @param A2   To incident matrix of author to paper.
#' @param citation    Character string, \dQuote{citation}, \dQuote{cocitation} and \dQuote{bcoupling}
#'
#' @return Return a type of citation network
#'
#' @references
#'
#' Batagelj, V., & Cerinšek, M. (2013). On bibliographic networks. Scientometrics, 96(3), 845–864.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A1 <- matrix(c(
#'   1, 0, 0, 0,
#'   0, 1, 0, 0,
#'   0, 1, 1, 1,
#'   0, 0, 0, 0,
#'   0, 0, 0, 1
#' ), byrow = TRUE, ncol = 4)
#'
#' A2 <- matrix(c(
#'   1, 1, 1, 0, 0,
#'   0, 0, 1, 0, 0,
#'   0, 0, 1, 1, 0,
#'   0, 0, 0, 1, 1
#' ), byrow = TRUE, ncol = 5)
#'
#' citation_norm(A1, A2)
#' @export

# TODO: Working progress (expand the measure)

citation_norm <- function(A1, A2, citation = "citation") {
  Ci <- A1 %*% A2
  Ci <- t(Ci) %*% Ci
  if (citation == "citation") {
    return(Ci)
  }

  if (citation == "cocitation") {
    D <- ifelse(rowSums(Ci) > 0, rowSums(Ci), 1)
    D <- diag(1 / D)
    Cin <- t(D %*% Ci)
    coCit <- Cin %*% Ci
    return(coCit)
  }

  if (citation == "bcoupling") {
    D <- ifelse(rowSums(Ci) > 0, rowSums(Ci), 1)
    D <- diag(1 / D)
    biCo <- Ci %*% t(Ci)
    biC <- D %*% biCo
    return(biC)
  }
}

#' Co‐occurrence
#'
#' Co‐occurrence matrix based on overlap function
#'
#' @param A  A matrix
#' @param similarity  The similarities available are either \code{Ochiai} (default) or \code{cosine}.
#' @param occurrence  Whether to treat the matrix as a two-mode structure (a.k.a. rectangular matrix, occurrence matrix, affiliation matrix, bipartite network)
#' @param projection Whether to apply a projection (inner product multiplication) to the matrix
#'
#' @return This function returns the normalisation of a matrix into a symmetrical co‐occurrence matrix
#'
#' @references
#'
#' Borgatti, S. P., Halgin, D. S., 2011. Analyzing affiliation networks. In: J. Scott and P. J. Carrington (Eds.) The Sage handbook of social network analysis (pp. 417-433), Sage.
#'
#' Zhou, Q., & Leydesdorff, L. (2016). The normalization of occurrence and Co-occurrence matrices in bibliometrics using Cosine similarities and Ochiai coefficients. Journal of the Association for Information Science and Technology, 67(11), 2805–2814. \url{https://doi.org/10.1002/asi.23603}
#'
#' @author Alejandro Espinosa-Rada

#' @examples
#'
#' A <- matrix(c(
#'   2, 0, 2,
#'   1, 1, 0,
#'   0, 3, 3,
#'   0, 2, 2,
#'   0, 0, 1
#' ),
#' nrow = 5, byrow = TRUE
#' )
#'
#' co_ocurrence(A)
#' @export

co_ocurrence <- function(A, similarity = c("ochiai", "cosine"),
                         occurrence = TRUE, projection = FALSE) {
  A <- as.matrix(A)
  similarity <- switch(similarity_option(similarity),
    "ochiai" = 1,
    "cosine" = 2
  )

  ### Occurrence matrix
  if (occurrence) {
    # OCHIAI
    if (similarity == 1) {
      Di <- rowSums(A)
      Dj <- colSums(A)
      Ab <- (t(A) %*% A)
      diag(Ab) <- colSums(A) # impute diagonal
      return(Ab / (sqrt(outer(Dj, Dj, "*"))))
    }
    # COSINE
    if (similarity == 2) {

      # coOC <- t(A) %*% A
      # D <- diag(coOC)
      # return(coOC / (sqrt(outer(D, D, "*"))))

      return((t(A) %*% A) / (sqrt(outer(colSums(A^2), colSums(A^2), "*"))))
    }
  }

  ### Co-occurrence matrix based on inner product
  if (!occurrence) {
    IN <- (t(A) %*% A)

    # OCHIAI:
    if (projection) {
      if (similarity == 1) {
        Di <- rowSums(A^2)
        Dj <- colSums(A^2)
        return(IN / (sqrt(outer(Dj, Dj, "*"))))
      }

      # COSINE:
      if (similarity == 2) {
        INb <- IN
        Di <- rowSums(INb^2)
        Dj <- colSums(INb^2)
        return((IN %*% t(IN)) / (sqrt(outer(Di, Dj, "*"))))
      }
    }
    ### Co-occurrence matrix based on minmax_overlap function/OCHIAI
    # COSINE:
    if (!projection) {

      # OCHIAI:
      if (similarity == 1) {
        D <- colSums(A)
        return(OVER / (sqrt(outer(D, D, "*"))))
      }

      if (similarity == 2) {
        OVER <- minmax_overlap(A, row = FALSE)
        OVERb <- OVER
        Di <- rowSums(OVERb^2)
        Dj <- colSums(OVERb^2)
        return(OVER %*% t(OVER) / (sqrt(outer(Di, Dj, "*"))))
      }
    }
  }
}

similarity_option <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  match.arg(arg = arg, choices = choices, several.ok = several.ok)
}

#' Jaccard similarity
#'
#' Jaccard similarity to identify the tie changes between two matrices.
#'
#' @param A  Binary matrix A
#' @param B  Binary matrix B
#' @param directed  Whether the matrix is symmetric
#' @param diag  Whether the diagonal should be considered
#' @param coparticipation  Select nodes that co-participate in both matrices
#'
#' @return The output are: \code{jaccard} = Jaccard similarity, \code{proportion} =
#' proportion among the ties present at a given observation of ties that
#' are also present in the other matrix, and \code{table} = a table with the
#' tie changes between matrices.
#'
#' If \code{coparticipation = TRUE}, then
#' also: \code{match} = number of nodes present in both matrices;
#'  \code{size_matrix1} = size of the first matrix;
#'  \code{size_matrix2} = size of the second matrix;
#' \code{coparticipation1} = percentage
#' of actors in the first matrix also present in the
#' second matrix; \code{coparticipation2} = percentage
#' of actors in the second matrix also present in the first matrix;
#' \code{overlap_actors} = overlap of nodes between two matrices
#'
#' @references
#'
#' Batagelj, V., and Bren, M. (1995). Comparing resemblance measures. Journal of Classification 12, 73–90.
#'
#'
#' @author Alejandro Espinosa-Rada

#' @examples
#'
#' A <- matrix(c(
#'   0, 1, 1, 0,
#'   1, 0, 0, 0,
#'   1, 0, 0, 0,
#'   0, 0, 1, 0
#' ), byrow = TRUE, ncol = 4)
#' B <- matrix(c(
#'   0, 1, 1, 0,
#'   1, 0, 0, 0,
#'   1, 0, 0, 0,
#'   0, 0, 0, 0
#' ), byrow = TRUE, ncol = 4)
#' jaccard(A, B, directed = TRUE)
#' @export

# TODO: expand for n periods
# TODO: expand for other similarities

jaccard <- function(A, B, directed = TRUE, diag = FALSE,
                    coparticipation = FALSE) {
  A <- as.matrix(A)
  B <- as.matrix(B)

  if (coparticipation) {
    if (all(rownames(A) != colnames(A))) stop("The names of rows and columns does not match")
    if (all(rownames(B) != colnames(B))) stop("The names of rows and columns does not match")

    n1t <- ncol(A)
    n2t <- ncol(B)
    name1 <- rownames(A) %in% rownames(B)
    name1 <- rownames(A)[name1 == TRUE]
    A <- A[rownames(A) %in% name1, rownames(A) %in% name1]
    B <- B[rownames(B) %in% name1, rownames(B) %in% name1]

    n1 <- ncol(A)
    n2 <- ncol(B)
  }

  if (any(abs(A > 1), na.rm = TRUE)) stop("The matrix should be binary")
  if (any(abs(B > 1), na.rm = TRUE)) stop("The matrix should be binary")
  if (!directed) {
    t <- table(A[lower.tri(A, diag = diag)], B[lower.tri(B, diag = diag)])
  } else {
    if (all(A[lower.tri(A)] == t(A)[lower.tri(A)])) warning("The matrix is symmetric")
    A <- c(A[lower.tri(A, diag = diag)], A[upper.tri(A, diag = diag)])
    B <- c(B[lower.tri(B, diag = diag)], B[upper.tri(B, diag = diag)])
    t <- table(A, B, useNA = c("always"))
  }
  n11 <- t[2, 2]
  n10 <- t[2, 1]
  n01 <- t[1, 2]
  n00 <- t[1, 1]

  if (coparticipation) {
    return(list(
      jaccard = n11 / (n10 + n01 + n11),
      proportion = n11 / (n10 + n11),
      table = t,
      coparticipation = cbind(
        match = n1,
        size_matrix1 = n1t,
        size_matrix2 = n2t,
        coparticipation1 = n1 / n1t,
        coparticipation2 = n2 / n2t,
        overlap_actors = ((n1 / n1t + n1 / n2t) / 2)
      )
    ))
  } else {
    return(list(
      jaccard = n11 / (n10 + n01 + n11),
      proportion = n11 / (n10 + n11),
      table = t
    ))
  }
}
