#' Conditional uniform graph test
#'
#' Compares a statistic of the observed network with its distribution in random networks that
#' share some of its features (Anderson, Butts and Carley, 1999; Wasserman and Faust, 1994).
#'
#' The random networks are drawn from a uniform distribution conditioned on:
#'
#' \code{size}: only the number of nodes, so every tie is present with probability one half,
#'
#' \code{edges}: the number of nodes and the number of ties,
#'
#' \code{dyad}: the dyad census, i.e. the number of mutual, asymmetric and null dyads (U|MAN).
#'
#' The test says whether the statistic is higher or lower than expected once those features are
#' taken into account. Conditioning on the dyad census, for instance, removes the tendency
#' towards reciprocity before looking at the triads.
#'
#' @param A   A square matrix
#' @param FUN   A function that takes a matrix and returns a single number
#' @param cmode   The feature that the random networks share with the observed one: \code{size}, \code{edges} (default) or \code{dyad}
#' @param reps   Number of random networks
#' @param digraph   Whether the matrix is directed or undirected
#' @param ...   Other arguments passed to \code{FUN}
#'
#' @return This function returns the observed statistic, the mean and the standard deviation of
#' the distribution, and the proportion of random networks with a statistic greater or equal,
#' and lower or equal, than the observed one.
#'
#' @references
#'
#' Anderson, B. S., Butts, C. and Carley, K. (1999). The interaction of size and density with graph-level indices. Social Networks, 21(3), 239–267. \doi{10.1016/S0378-8733(99)00011-8}
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   1, 0, 1, 0, 0, 0,
#'   1, 1, 0, 1, 0, 0,
#'   0, 0, 1, 0, 1, 1,
#'   0, 0, 0, 1, 0, 1,
#'   0, 0, 0, 1, 1, 0
#' ), byrow = TRUE, ncol = 6)
#' rownames(A) <- letters[1:nrow(A)]
#' colnames(A) <- rownames(A)
#'
#' set.seed(18051889)
#' cug_test(A,
#'   FUN = function(x) trans_coef(x, method = "global"),
#'   cmode = "edges", reps = 100, digraph = FALSE
#' )
#' @export

cug_test <- function(A, FUN, cmode = c("edges", "size", "dyad"), reps = 1000,
                     digraph = TRUE, ...) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  cmode <- match.arg(cmode)
  if (cmode == "dyad" & !digraph) stop("The dyad census conditions the mutual and asymmetric dyads of a directed network")

  observed <- FUN(A, ...)
  if (length(observed) != 1) stop("FUN should return a single number")

  distribution <- rep(NA, reps)
  for (i in seq_len(reps)) {
    distribution[i] <- FUN(rand_cug(A, cmode = cmode, digraph = digraph), ...)
  }

  return(list(
    observed = observed,
    mean = mean(distribution, na.rm = TRUE),
    sd = stats::sd(distribution, na.rm = TRUE),
    p_greater = mean(distribution >= observed, na.rm = TRUE),
    p_lower = mean(distribution <= observed, na.rm = TRUE),
    distribution = distribution,
    cmode = cmode
  ))
}

# One random network that shares the conditioned features with the observed one
rand_cug <- function(A, cmode, digraph) {
  n <- nrow(A)
  B <- matrix(0, n, n, dimnames = dimnames(A))

  if (digraph) {
    cells <- which(row(A) != col(A))
  } else {
    cells <- which(upper.tri(A))
  }

  if (cmode == "size") {
    B[cells] <- stats::rbinom(length(cells), 1, 0.5)
  }
  if (cmode == "edges") {
    ties <- 1 * (A[cells] != 0)
    B[cells] <- sample(ties)
  }
  if (cmode == "dyad") {
    # The states of the dyads are shuffled, and the direction of the
    # asymmetric dyads is chosen at random
    pairs <- which(upper.tri(A), arr.ind = TRUE)
    state <- rep("n", nrow(pairs))
    state[A[pairs] != 0 & A[pairs[, c(2, 1), drop = FALSE]] != 0] <- "m"
    state[xor(A[pairs] != 0, A[pairs[, c(2, 1), drop = FALSE]] != 0)] <- "a"
    state <- sample(state)

    mutual <- pairs[state == "m", , drop = FALSE]
    B[mutual] <- 1
    B[mutual[, c(2, 1), drop = FALSE]] <- 1
    asymmetric <- pairs[state == "a", , drop = FALSE]
    flip <- stats::runif(nrow(asymmetric)) < 0.5
    asymmetric[flip, ] <- asymmetric[flip, c(2, 1)]
    B[asymmetric] <- 1
    return(B)
  }

  if (!digraph) {
    B <- B + t(B)
  }
  B
}


#' QAP correlation
#'
#' Correlation between two matrices, with a test based on the permutation of the nodes
#' (Hubert and Schultz, 1976; Krackhardt, 1987).
#'
#' The ties of a network are not independent, so the usual test of a correlation does not
#' apply. The quadratic assignment procedure compares the observed correlation with the
#' correlations obtained after permuting the rows and the columns of one of the matrices at the
#' same time, which keeps its structure while breaking its association with the other matrix.
#'
#' @param A   A square matrix
#' @param B   A square matrix of the same order
#' @param reps   Number of permutations
#' @param diag   Whether the diagonal is considered
#' @param method   Correlation coefficient: \code{pearson} (default), \code{spearman} or \code{kendall}
#'
#' @return This function returns the observed correlation and the proportion of permutations with a correlation greater or equal, lower or equal, and larger in absolute value, than the observed one.
#'
#' @references
#'
#' Hubert, L. and Schultz, J. (1976). Quadratic assignment as a general data analysis strategy. British Journal of Mathematical and Statistical Psychology, 29(2), 190–241. \doi{10.1111/j.2044-8317.1976.tb00714.x}
#'
#' Krackhardt, D. (1987). QAP partialling as a test of spuriousness. Social Networks, 9(2), 171–186. \doi{10.1016/0378-8733(87)90012-8}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0,
#'   1, 0, 1, 0,
#'   1, 1, 0, 1,
#'   0, 0, 1, 0
#' ), byrow = TRUE, ncol = 4)
#' B <- matrix(c(
#'   0, 1, 0, 0,
#'   1, 0, 1, 0,
#'   0, 1, 0, 1,
#'   0, 0, 1, 0
#' ), byrow = TRUE, ncol = 4)
#'
#' set.seed(18051889)
#' qap_cor(A, B, reps = 100)
#' @export

qap_cor <- function(A, B, reps = 1000, diag = FALSE, method = c("pearson", "spearman", "kendall")) {
  A <- as.matrix(A)
  B <- as.matrix(B)
  if (!all(dim(A) == dim(B))) stop("Non-conformable arrays")
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  method <- match.arg(method)

  cells <- rep(TRUE, length(A))
  if (!diag) {
    cells <- row(A) != col(A)
  }
  observed <- stats::cor(A[cells], B[cells], method = method)

  distribution <- rep(NA, reps)
  for (i in seq_len(reps)) {
    order_nodes <- sample(nrow(A))
    permuted <- B[order_nodes, order_nodes]
    distribution[i] <- stats::cor(A[cells], permuted[cells], method = method)
  }

  return(list(
    correlation = observed,
    p_greater = mean(distribution >= observed),
    p_lower = mean(distribution <= observed),
    p_two_sided = mean(abs(distribution) >= abs(observed)),
    distribution = distribution
  ))
}


#' QAP regression
#'
#' Regression between matrices, with a test based on the permutation of the nodes
#' (Krackhardt, 1988; Dekker, Krackhardt and Snijders, 2007).
#'
#' The coefficients are those of an ordinary regression (or a logistic regression when
#' \code{family = "binomial"}) of the ties of \code{Y} on the ties of the matrices in \code{X}.
#' As the ties are not independent, the standard errors of the regression do not apply, and the
#' coefficients are compared with the ones obtained after permuting the nodes.
#'
#' With \code{method = "y"} the rows and columns of \code{Y} are permuted. With
#' \code{method = "dsp"} (default) the double semi-partialling of Dekker et al. (2007) is used:
#' each predictor is regressed on the other predictors, and the residuals of that regression are
#' permuted, which behaves better when the predictors are correlated with each other. The
#' intercept is not permuted by the double semi-partialling, so its p-values are not returned.
#'
#' @param Y   A square matrix with the dependent relation
#' @param X   A list of square matrices with the independent relations
#' @param reps   Number of permutations
#' @param family   \code{gaussian} for a linear regression (default) or \code{binomial} for a logistic regression
#' @param method   Whether to permute the dependent matrix (\code{y}) or the residuals of each predictor (\code{dsp}, default)
#' @param diag   Whether the diagonal is considered
#'
#' @return This function returns the coefficients, the proportion of permutations with a coefficient greater or equal, lower or equal, and larger in absolute value, than the observed one, and the fit of the model.
#'
#' @references
#'
#' Dekker, D., Krackhardt, D. and Snijders, T. A. B. (2007). Sensitivity of MRQAP tests to collinearity and autocorrelation conditions. Psychometrika, 72(4), 563–581. \doi{10.1007/s11336-007-9016-1}
#'
#' Krackhardt, D. (1988). Predicting with networks: Nonparametric multiple regression analysis of dyadic data. Social Networks, 10(4), 359–381. \doi{10.1016/0378-8733(88)90004-4}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' set.seed(18051889)
#' Y <- matrix(c(
#'   0, 1, 1, 0, 0,
#'   1, 0, 1, 0, 0,
#'   1, 1, 0, 1, 0,
#'   0, 0, 1, 0, 1,
#'   0, 0, 0, 1, 0
#' ), byrow = TRUE, ncol = 5)
#' X1 <- matrix(c(
#'   0, 1, 0, 0, 0,
#'   1, 0, 1, 0, 0,
#'   0, 1, 0, 1, 0,
#'   0, 0, 1, 0, 1,
#'   0, 0, 0, 1, 0
#' ), byrow = TRUE, ncol = 5)
#'
#' qap_lm(Y, list(distance = X1), reps = 100)
#' @export

qap_lm <- function(Y, X, reps = 1000, family = c("gaussian", "binomial"),
                   method = c("dsp", "y"), diag = FALSE) {
  Y <- as.matrix(Y)
  if (!is.list(X)) {
    X <- list(X)
  }
  X <- lapply(X, as.matrix)
  for (k in seq_along(X)) {
    if (!all(dim(X[[k]]) == dim(Y))) stop("Non-conformable arrays")
  }
  if (nrow(Y) != ncol(Y)) stop("Matrix should be square")
  family <- match.arg(family)
  method <- match.arg(method)
  if (is.null(names(X))) {
    names(X) <- paste0("x", seq_along(X))
  }

  cells <- rep(TRUE, length(Y))
  if (!diag) {
    cells <- row(Y) != col(Y)
  }
  y <- Y[cells]
  x <- sapply(X, function(m) m[cells])

  observed <- qap_coef(y, x, family)

  distribution <- matrix(NA, reps, length(observed))
  colnames(distribution) <- names(observed)
  for (i in seq_len(reps)) {
    order_nodes <- sample(nrow(Y))
    if (method == "y") {
      permuted <- Y[order_nodes, order_nodes]
      distribution[i, ] <- qap_coef(permuted[cells], x, family)
    } else {
      # Double semi-partialling: the residuals of each predictor are permuted
      x_perm <- x
      for (k in seq_along(X)) {
        if (length(X) == 1) {
          residual <- X[[k]]
        } else {
          others <- sapply(X[-k], function(m) m[cells])
          fitted <- stats::lm.fit(cbind(1, others), x[, k])
          residual <- matrix(0, nrow(Y), ncol(Y))
          residual[cells] <- stats::residuals(fitted)
        }
        residual <- residual[order_nodes, order_nodes]
        x_perm[, k] <- residual[cells]
      }
      distribution[i, ] <- qap_coef(y, x_perm, family)
    }
  }

  coefficients <- data.frame(
    coefficient = observed,
    p_greater = colMeans(distribution >= matrix(observed, reps, length(observed), byrow = TRUE)),
    p_lower = colMeans(distribution <= matrix(observed, reps, length(observed), byrow = TRUE)),
    p_two_sided = colMeans(abs(distribution) >= abs(matrix(observed, reps, length(observed), byrow = TRUE)))
  )

  if (method == "dsp") {
    # The intercept is not permuted by the double semi-partialling
    coefficients[1, c("p_greater", "p_lower", "p_two_sided")] <- NA
  }

  if (family == "gaussian") {
    fit <- summary(stats::lm(y ~ x))$r.squared
  } else {
    model <- stats::glm(y ~ x, family = stats::binomial())
    fit <- 1 - (model$deviance / model$null.deviance)
  }

  return(list(coefficients = coefficients, fit = fit, distribution = distribution))
}

# Coefficients of the regression of the ties
qap_coef <- function(y, x, family) {
  if (family == "gaussian") {
    coefficients <- stats::lm.fit(cbind(intercept = 1, x), y)$coefficients
  } else {
    coefficients <- stats::glm.fit(cbind(intercept = 1, x), y, family = stats::binomial())$coefficients
  }
  names(coefficients) <- c("intercept", colnames(x))
  coefficients
}
