#' Fractional approach
#'
#' Matrix transformation from incident matrices to citation networks, fractional counting for co-citation or fractional counting for bibliographic coupling
#'
#' @param A1   From incident matrix (e.g. paper and authors)
#' @param A2   To incident matrix (e.g. author to paper)
#' @param approach    Character string, \dQuote{citation}, \dQuote{cocitation} and \dQuote{bcoupling}
#'
#' @return Return a type of "citation network"
#'
#' @references
#'
#' Batagelj, V. (2020). Analysis of the Southern women network using fractional approach. Social Networks, 68, 229-236 \url{https://doi.org/10.1016/j.socnet.2021.08.001}
#'
#' Batagelj, V., & Cerinšek, M. (2013). On bibliographic networks. Scientometrics, 96(3), 845–864. \url{https://doi.org/10.1007/s11192-012-0940-1}
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
#' fractional_approach(A1, A2)
#' @export

fractional_approach <- function(A1, A2, approach = c("citation", "cocitation", "bcoupling")) {
  A1 <- as.matrix(A1)
  A2 <- as.matrix(A2)

  similarity <- switch(similarity_option(approach),
    "citation" = 1,
    "cocitation" = 2,
    "bcoupling" = 3
  )

  Ci <- A1 %*% A2
  Ci <- t(Ci) %*% Ci

  # Citation Networks
  if (similarity == 1) {
    return(Ci)
  }

  # Co-citations
  if (similarity == 2) {
    D <- ifelse(rowSums(Ci) > 0, rowSums(Ci), 1)
    D <- diag(1 / D)
    Cin <- t(D %*% Ci)
    coCit <- Cin %*% Ci
    return(coCit)
  }

  # Bibliographic coupling
  if (similarity == 3) {
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
#' Jaccard similarity identifies the changes of ties between two matrices.
#'
#' @param A  Binary matrix A
#' @param B  Binary matrix B
#' @param directed  Whether the matrix is symmetric
#' @param diag  Whether the diagonal should be considered
#' @param coparticipation  Select nodes that co-participate in both matrices
#' @param bipartite  Whether the matrix is incident
#'
#' @return The output are: \code{jaccard} = Jaccard similarity, \code{proportion} =
#' proportion among the ties present at a given observation of ties that
#' are also present in the other matrix, and \code{table} = a table with the
#' tie changes between matrices.
#'
#' If \code{coparticipation = TRUE}, then
#' also: \code{match} = The number of nodes present in both matrices;
#'  \code{size_matrix1} = The size of the first matrix;
#'  \code{size_matrix2} = The size of the second matrix;
#' \code{coparticipation1} = The percentage of nodes in the first matrix also present in the second matrix;
#' \code{coparticipation2} = The percentage of nodes in the second matrix also present in the first matrix:
#' \code{overlap_actors} = Overlap of nodes between two matrices
#'
#' #' If \code{coparticipation = TRUE} and \code{bipartite = TRUE}, then
#' also: \code{matchM1} = The number of nodes in the first 'mode' present in both matrices;
#' \code{matchM2} = The number of nodes in the second 'mode' present in both matrices;
#'  \code{size_matrix1_M1} = The number of nodes in the first 'mode' of the first matrix;
#'  \code{size_matrix1_M2} = The number of nodes in the second 'mode' of the first matrix;
#'  \code{size_matrix2_M1} = The number of nodes in the first 'mode' of the second matrix;
#'  \code{size_matrix2_M2} = The number of nodes in the second 'mode' of the second matrix;
#' \code{coparticipation1_M2} = The percentage of nodes of the first 'mode' in the first matrix present in the second matrix.
#' \code{coparticipation1_M2} = The percentage of nodes of the second 'mode' in the first matrix present in the second matrix.
#' \code{coparticipation2_M1} = The percentage of nodes of the first 'mode' in the second matrix present in the first matrix.
#'  \code{coparticipation2_M2} = The percentage of nodes of the second 'mode' in the second matrix present in the first matrix.
#' \code{overlap_actors_M1} = Overlap between two matrices (nodes of the first 'mode')
#' \code{overlap_actors_M2} = Overlap between two matrices (nodes of the second 'mode')
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
                    coparticipation = FALSE, bipartite = FALSE) {
  A <- as.matrix(A)
  B <- as.matrix(B)
  if (any(abs(A > 1), na.rm = TRUE)) stop("The matrix should be binary")
  if (any(abs(B > 1), na.rm = TRUE)) stop("The matrix should be binary")

  if (coparticipation) {
    if (!bipartite) {
      if (all(rownames(A) != colnames(A))) stop("The names of rows and columns do not match")
      if (all(rownames(B) != colnames(B))) stop("The names of rows and columns do not match")

      n1t <- ncol(A)
      n2t <- ncol(B)
      name1 <- rownames(A) %in% rownames(B)
      name1 <- rownames(A)[name1 == TRUE]
      A <- A[rownames(A) %in% name1, rownames(A) %in% name1]
      B <- B[rownames(B) %in% name1, rownames(B) %in% name1]

      n1 <- ncol(A)
      n2 <- ncol(B)
    } else {
      # bipartite
      n1_at <- nrow(A)
      n1_bt <- ncol(A)
      n2_at <- nrow(B)
      n2_bt <- ncol(B)

      name1a <- rownames(A) %in% rownames(B)
      name1a <- rownames(A)[name1a == TRUE]

      name1b <- colnames(A) %in% colnames(B)
      name1b <- colnames(A)[name1b == TRUE]

      A <- A[rownames(A) %in% name1a, colnames(A) %in% name1b]
      B <- B[rownames(B) %in% name1a, colnames(B) %in% name1b]

      n1a <- nrow(A)
      n1b <- ncol(A)
    }
  }

  if (bipartite) {
    if (!coparticipation) {
      if (ncol(A) != ncol(B)) {
        stop("The matrices have different dimensions")
      } else {
        if (all(rownames(A) != rownames(B))) stop("The names of nodes do not match")
      }
      if (nrow(A) != nrow(B)) {
        stop("The matrices have different dimensions")
      } else {
        if (all(colnames(B) != colnames(B))) stop("The names of nodes do not match")
      }
    }

    t <- table(A, B, useNA = c("always"))
  } else {
    if (!directed) {
      t <- table(A[lower.tri(A, diag = diag)], B[lower.tri(B, diag = diag)])
    } else {
      if (all(A[lower.tri(A)] == t(A)[lower.tri(A)])) message("The matrix is symmetric")
      A <- c(A[lower.tri(A, diag = diag)], A[upper.tri(A, diag = diag)])
      B <- c(B[lower.tri(B, diag = diag)], B[upper.tri(B, diag = diag)])
      t <- table(A, B, useNA = c("always"))
    }
  }
  n11 <- t[2, 2]
  n10 <- t[2, 1]
  n01 <- t[1, 2]
  n00 <- t[1, 1]

  if (coparticipation) {
    if (!bipartite) {
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
      # bipartite
      return(list(
        jaccard = n11 / (n10 + n01 + n11),
        proportion = n11 / (n10 + n11),
        table = t,
        coparticipation = cbind(
          matchM1 = n1a, # match mode 1
          matchM2 = n1b, # match mode 2

          size_matrix1_M1 = n1_at, # size matrix rows A
          size_matrix1_M2 = n1_bt, # size matrix columns A
          size_matrix2_M1 = n2_at, # size matrix rows B
          size_matrix2_M2 = n2_bt, # size matrix columns B

          coparticipation1_M1 = n1a / n1_at,
          coparticipation1_M2 = n1b / n1_bt,
          coparticipation2_M1 = n1a / n2_at,
          coparticipation2_M2 = n1b / n2_bt,
          overlap_actors_M1 = ((n1a / n1_at + n1a / n2_at) / 2), # overlap
          overlap_actors_M2 = ((n1b / n1_bt + n1b / n2_bt) / 2)
        )
      ))
    }
  } else {
    return(list(
      jaccard = n11 / (n10 + n01 + n11),
      proportion = n11 / (n10 + n11),
      table = t
    ))
  }
}

#' Structural similarities
#'
#' @param A  A matrix
#' @param method  The similarities/distance currently available are either \code{Euclidean} (default) or \code{Jaccard}.
#'
#' @return This function returns a distance matrix between nodes of the same matrix.
#'
#' @references
#' 
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 0, 0, 1,
#'   0, 0, 0, 1, 1,
#'   0, 1, 0, 0, 1,
#'   0, 0, 1, 1, 0,
#'   0, 1, 0, 0, 0
#' ), nrow = 5, ncol = 5, byrow = TRUE)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#' dist_sim_matrix(A, method = "jaccard")
#'
#' A <- matrix(c(
#'   0, 0, 3, 0, 5,
#'   0, 0, 2, 0, 4,
#'   5, 4, 0, 4, 0,
#'   0, 3, 0, 1, 0,
#'   0, 0, 0, 0, 2
#' ), nrow = 5, ncol = 5, byrow = TRUE)
#' dist_sim_matrix(A, method = "euclidean")
#' @export

dist_sim_matrix <- function(A, method = c("euclidean", "jaccard")) {
  A <- as.matrix(A)
  method <- switch(sim_method(method),
    "euclidean" = 1,
    "jaccard" = 2
  )
  profile <- list()
  profile2 <- list()

  if (method == 1) { # euclidean
    for (i in 1:nrow(A)) {
      for (j in 1:ncol(A)) {
        profile[[j]] <- sqrt(sum((A[i, ] - A[j, ])^2))
      }
      profile2[[i]] <- unlist(profile)
    }
    m1 <- do.call(rbind, profile2)
    return(m1)
  }

  if (method == 2) { # jaccard
    for (i in 1:nrow(A)) {
      for (j in 1:ncol(A)) {
        t <- table(A[i, ], A[j, ])
        n11 <- t[2, 2]
        n10 <- t[2, 1]
        n01 <- t[1, 2]
        n00 <- t[1, 1]
        profile[[j]] <- n11 / (n11 + n01 + n10)
      }
      profile2[[i]] <- unlist(profile)
    }
    m1 <- do.call(rbind, profile2)
    return(1 - m1)
  }
}

sim_method <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  match.arg(arg = arg, choices = choices, several.ok = several.ok)
}
