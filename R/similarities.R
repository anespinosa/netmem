#' Fractional approach
#'
#' Citation, co-citation and bibliographic coupling networks with full or fractional counting (Batagelj, 2020).
#'
#' @details
#' \code{A1} is a citation network \eqn{Ci} between works, where \code{A1[p, q] = 1} when the work \eqn{p}
#' cites the work \eqn{q}. With fractional counting each work has a total weight of one, which is divided
#' equally among the works it cites: \eqn{Cin = D \cdot Ci}, where \eqn{D} is the diagonal matrix of one over
#' the number of references of each work (one when it has none). Fractional counting prevents the works with
#' many references, such as reviews, from dominating the result (Batagelj, 2020).
#'
#' \code{approach = "cocitation"}: \eqn{Ci^T Ci}, the number of works that cite both works, or with fractional
#' counting \eqn{Cin^T Cin}, in which each citing work contributes a total of one.
#'
#' \code{approach = "bcoupling"}: \eqn{Ci \cdot Ci^T}, the number of works cited by both works. Fractional counting
#' cannot be applied in the same way to bibliographic coupling (Batagelj, 2020: 631), so it gives
#' \eqn{biC = Cin \cdot Ci^T}, the proportion of the references of \eqn{p} that it shares with \eqn{q}, which is
#' not symmetric. \code{symmetric} turns it into a symmetric similarity: the \code{average}, the \code{minimum},
#' the \code{maximum}, the \code{geometric} mean (the cosine of Salton), the \code{harmonic} mean, or the
#' \code{jaccard} index, the shared references divided by the references of either work.
#'
#' \code{approach = "citation"}: the citations between the authors of the works, given the authorship matrix
#' \eqn{WA} in \code{A2} (works in rows, authors in columns): \eqn{WA^T \cdot Ci \cdot WA}, the number of times the
#' works of an author cite the works of another. With fractional counting each work is divided equally among its
#' authors, \eqn{WAn^T \cdot Ci \cdot WAn}, so that the total of the network is the number of citations.
#'
#' Which count should be conserved guides the choice between the two protocols (Prathap and Mukherjee, 2020): full
#' counting conserves the number of paths between the nodes, and fractional counting the number of nodes (works).
#'
#' @param A1   A citation network between works, where \code{A1[p, q] = 1} if the work p cites the work q
#' @param A2   For \code{approach = "citation"}, the authorship matrix, with the works in rows and the authors in columns
#' @param approach    Character string, \dQuote{citation}, \dQuote{cocitation} and \dQuote{bcoupling}
#' @param fractional  Whether to use fractional counting (default) or full counting
#' @param symmetric   For fractional bibliographic coupling, the symmetric similarity: \code{none} (default, the proportion of shared references), \code{average}, \code{minimum}, \code{maximum}, \code{geometric}, \code{harmonic} or \code{jaccard}
#'
#' @return Return the citation network between authors, the co-citation network or the bibliographic coupling network.
#'
#' @references
#'
#' Batagelj, V. (2020). On fractional approach to analysis of linked networks. Scientometrics, 123(2), 621-633. \doi{10.1007/s11192-020-03383-y}
#'
#' Batagelj, V. (2022). Analysis of the Southern women network using fractional approach. Social Networks, 68, 229-236 \doi{10.1016/j.socnet.2021.08.001}
#'
#' Batagelj, V., & Cerinšek, M. (2013). On bibliographic networks. Scientometrics, 96(3), 845–864. \doi{10.1007/s11192-012-0940-1}
#'
#' Prathap, G., & Mukherjee, S. (2020). Letter to the Editor: Comments on the paper of Batagelj—on fractional approach to analysis of linked networks. Scientometrics, 124(3), 2717–2722. \doi{10.1007/s11192-020-03541-2}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' # Five works: w1 cites w2 and w3, w4 cites w2, w3 and w5, w5 cites w3
#' Ci <- matrix(c(
#'   0, 1, 1, 0, 0,
#'   0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0,
#'   0, 1, 1, 0, 1,
#'   0, 0, 1, 0, 0
#' ), byrow = TRUE, ncol = 5)
#' rownames(Ci) <- colnames(Ci) <- paste0("w", 1:5)
#'
#' # Authors of the works
#' WA <- matrix(c(
#'   1, 1, 0,
#'   0, 1, 0,
#'   0, 0, 1,
#'   1, 0, 0,
#'   0, 0, 1
#' ), byrow = TRUE, ncol = 3)
#' rownames(WA) <- rownames(Ci)
#' colnames(WA) <- c("a1", "a2", "a3")
#'
#' fractional_approach(Ci, WA, approach = "citation")
#' fractional_approach(Ci, approach = "cocitation")
#' fractional_approach(Ci, approach = "bcoupling", symmetric = "geometric")
#' @export

fractional_approach <- function(A1, A2 = NULL, approach = c("citation", "cocitation", "bcoupling"),
                                fractional = TRUE,
                                symmetric = c("none", "average", "minimum", "maximum", "geometric", "harmonic", "jaccard")) {
  approach <- match.arg(approach)
  symmetric <- match.arg(symmetric)
  Ci <- as.matrix(A1)
  if (any(is.na(Ci) == TRUE)) {
    Ci <- ifelse(is.na(Ci), 0, Ci)
  }
  if (nrow(Ci) != ncol(Ci)) stop("A1 should be a square citation network between works")

  # Each row divided by its sum, or by one when the work has no ties
  row_normalize <- function(M) {
    total <- rowSums(M)
    total[total == 0] <- 1
    M / total
  }

  # Citations between authors
  if (approach == "citation") {
    if (is.null(A2)) stop("The authorship matrix should be given in A2")
    WA <- as.matrix(A2)
    if (any(is.na(WA) == TRUE)) {
      WA <- ifelse(is.na(WA), 0, WA)
    }
    if (nrow(WA) != nrow(Ci)) stop("A2 should have one row for each work of A1")
    if (fractional) {
      WA <- row_normalize(WA)
    }
    return(t(WA) %*% Ci %*% WA)
  }

  # Co-citation
  if (approach == "cocitation") {
    if (fractional) {
      Cin <- row_normalize(Ci)
      return(t(Cin) %*% Cin)
    }
    return(t(Ci) %*% Ci)
  }

  # Bibliographic coupling
  if (!fractional) {
    return(Ci %*% t(Ci))
  }
  biC <- row_normalize(Ci) %*% t(Ci)
  if (symmetric == "none") {
    return(biC)
  }
  if (symmetric == "average") {
    return((biC + t(biC)) / 2)
  }
  if (symmetric == "minimum") {
    return(pmin(biC, t(biC)))
  }
  if (symmetric == "maximum") {
    return(pmax(biC, t(biC)))
  }
  if (symmetric == "geometric") {
    return(sqrt(biC * t(biC)))
  }
  if (symmetric == "harmonic") {
    H <- 2 * biC * t(biC) / (biC + t(biC))
    H[is.nan(H)] <- 0
    return(H)
  }
  # Jaccard: shared references over the references of either work
  shared <- Ci %*% t(Ci)
  references <- rowSums(Ci)
  J <- shared / (outer(references, references, "+") - shared)
  J[is.nan(J)] <- 0
  return(J)
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
#' Zhou, Q., & Leydesdorff, L. (2016). The normalization of occurrence and Co-occurrence matrices in bibliometrics using Cosine similarities and Ochiai coefficients. Journal of the Association for Information Science and Technology, 67(11), 2805–2814. \doi{10.1002/asi.23603}
#'
#' @author Alejandro Espinosa-Rada

#' @examples
#'
#' A <- matrix(
#'   c(
#'     2, 0, 2,
#'     1, 1, 0,
#'     0, 3, 3,
#'     0, 2, 2,
#'     0, 0, 1
#'   ),
#'   nrow = 5, byrow = TRUE
#' )
#'
#' co_occurrence(A)
#' @export

co_occurrence <- function(A, similarity = c("ochiai", "cosine"),
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
      OVER <- minmax_overlap(A, row = FALSE)
      # OCHIAI:
      if (similarity == 1) {
        D <- colSums(A)
        return(OVER / (sqrt(outer(D, D, "*"))))
      }

      if (similarity == 2) {
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
#' @param directed  Whether the matrix is directed (asymmetric)
#' @param diag  Whether the diagonal should be considered
#' @param coparticipation  Select nodes that co-participate in both matrices
#' @param bipartite  Whether the matrix is incidence
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
#' If \code{coparticipation = TRUE} and \code{bipartite = TRUE}, then
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
#' @author Alejandro Espinosa-Rada
#'
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
        if (any(rownames(A) != rownames(B))) stop("The names of nodes do not match")
      }
      if (nrow(A) != nrow(B)) {
        stop("The matrices have different dimensions")
      } else {
        if (any(colnames(A) != colnames(B))) stop("The names of nodes do not match")
      }
    }

    a <- c(A)
    b <- c(B)
  } else {
    if (!directed) {
      a <- A[lower.tri(A, diag = diag)]
      b <- B[lower.tri(B, diag = diag)]
    } else {
      if (all(A[lower.tri(A)] == t(A)[lower.tri(A)])) message("The matrix is symmetric")
      a <- c(A[lower.tri(A, diag = diag)], A[upper.tri(A, diag = diag)])
      b <- c(B[lower.tri(B, diag = diag)], B[upper.tri(B, diag = diag)])
    }
  }
  # The table has both values even when a matrix has only zeros or only ones
  t <- table(factor(a, levels = c(0, 1)), factor(b, levels = c(0, 1)), useNA = "ifany")
  n11 <- t["1", "1"]
  n10 <- t["1", "0"]
  n01 <- t["0", "1"]
  n00 <- t["0", "0"]

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
#' In the literature of social network, Euclidean distance (Burt, 1976) or correlations (Wasserman and Faust, 1994) were considered as measures of structural equivalence.
#'
#' @param A  A matrix
#' @param method  The similarities/distance currently available are either \code{Euclidean} (default), \code{Hamming}, or \code{Jaccard}.
#' @param bipartite  Whether the object is an incidence matrix
#'
#' @return This function returns a distance matrix between nodes of the same matrix.
#'
#' @references
#'
#' Burt, Ronald S. (1976) Positions in networks. Social Forces, 55(1): 93-122.
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
#'
#' # Several relations are compared at the same time, stacking the rows and the
#' # columns of every matrix
#' B <- matrix(c(
#'   0, 1, 0, 0, 1,
#'   1, 0, 0, 0, 1,
#'   0, 0, 0, 1, 0,
#'   0, 0, 1, 0, 0,
#'   1, 1, 0, 0, 0
#' ), nrow = 5, ncol = 5, byrow = TRUE)
#' dist_sim_matrix(list(A, B), method = "euclidean")
#' @export

dist_sim_matrix <- function(A, method = c("euclidean", "hamming", "jaccard"),
                            bipartite = FALSE) {
  if (is.list(A)) {
    # The profile of a node stacks its rows and its columns in every relation,
    # so the nodes are compared across all of them at once
    A <- lapply(A, as.matrix)
    for (k in seq_along(A)) {
      if (nrow(A[[k]]) != nrow(A[[1]])) stop("The matrices should have the same number of rows")
    }
    square <- sapply(A, function(m) nrow(m) == ncol(m))
    profile <- do.call(cbind, c(A, lapply(A[square], t)))
    rownames(profile) <- rownames(A[[1]])
    A <- profile
    bipartite <- TRUE # the profile is rectangular, the rows are compared
  }
  A <- as.matrix(A)
  if (!bipartite) {
    if (ncol(A) != nrow(A)) message("The object is an incidence matrix. The `bipartite=TRUE` parameter should be specified.")
  }

  method <- sim_method(method)
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }

  # The rows are compared pair by pair: the Euclidean distance, the Hamming
  # distance (the number of different cells) or the Jaccard distance, one minus
  # the proportion of shared ties among the ties of either row (zero for two
  # rows without ties)
  n <- nrow(A)
  D <- matrix(0, n, n, dimnames = list(rownames(A), rownames(A)))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      a <- A[i, ]
      b <- A[j, ]
      if (method == "euclidean") {
        D[i, j] <- sqrt(sum((a - b)^2))
      }
      if (method == "hamming") {
        D[i, j] <- sum(a != b)
      }
      if (method == "jaccard") {
        union <- sum(a != 0 | b != 0)
        D[i, j] <- if (union == 0) 0 else 1 - sum(a != 0 & b != 0) / union
      }
    }
  }
  return(D)
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


#' Bonacich normalization
#'
#' The function provide a normalisation provided by Bonacich (1972).
#'
#' @param A  An incidence matrix
#' @param projection  Whether to normalise by \code{rows} (default), or \code{columns} of the matrix.
#' @param normalisation  Normalise the measure
#'
#' @return This function returns the Bonacich normalisation.
#'
#' @references
#'
#' Bonacich, P. (1972). Factoring and weighting approaches to status scores and clique identification. Journal of Mathematical Sociology, 2: 112-120.
#'
#' @source Adapted from Borgatti, S., Everett, M., Johnson, J. and Agneessens, P. (2022) Analyzing Social Networks Using R. Sage.
#'
#' @examples
#' A <- matrix(
#'   c(
#'     1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0,
#'     1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
#'     0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,
#'     1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
#'     0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,
#'     0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0,
#'     0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0,
#'     0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1,
#'     0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1,
#'     0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1,
#'     0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0,
#'     0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0
#'   ),
#'   byrow = TRUE, ncol = 14
#' )
#' bonacich_norm(A)
#' @export

bonacich_norm <- function(A, projection = c("rows", "columns"),
                          normalisation = FALSE) {
  projection <- switch(projection_direction(projection),
    "rows" = 1,
    "columns" = 2
  )

  if (projection == 1) {
    P <- matrix_projection(A)[[2]]
    n <- ncol(A)
  }
  if (projection == 2) {
    P <- matrix_projection(A)[[1]]
    n <- nrow(A)
  }

  M <- matrix(0, nrow(P), ncol(P))
  for (i in 1:nrow(P)) {
    for (j in i:ncol(P)) {
      temp1 <- P[i, j] * (n + P[i, j] - P[i, i] - P[j, j])
      temp2 <- (P[i, i] - P[i, j]) * (P[j, j] - P[i, j])

      if (temp1 == temp2) {
        M[i, j] <- 0.5
      } else {
        M[i, j] <- (temp1 - sqrt(temp1 * temp2)) / (temp1 - temp2)
      }
      M[j, i] <- M[i, j]
    }
    M[i, i] <- 1
  }

  if (projection == 1) {
    if (!is.null(rownames(A))) {
      rownames(M) <- rownames(A)
      colnames(M) <- rownames(A)
    }
  }
  if (projection == 2) {
    if (!is.null(colnames(A))) {
      rownames(M) <- colnames(A)
      colnames(M) <- colnames(A)
    }
  }
  if (normalisation) {
    M <- M * 100
  }

  return(M)
}


projection_direction <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  match.arg(arg = arg, choices = choices, several.ok = several.ok)
}
