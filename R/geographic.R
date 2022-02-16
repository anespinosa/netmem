#' Geographical distances
#'
#' This function calculate some geographical distances considering a list of places specifying their latitud and longitud. The function currently works for degree decimal or radians formats.
#'
#' @param latitude   A vector with latitude
#' @param longitud   A vector with longitud
#' @param method   Whether to use the Spherical Law of Cosines \code{spherical} (default), Haversine formula \code{harvesine}, Manhattan Distance \code{manhattan} or Minkoowski distance \code{minkowski}
#' @param places   A vector with the names of the places
#' @param dd_to_radians  Whether to transform degree decimal format to radians
#' @param p  Parameter p for the estimation of Minkowski distance (default = 2, which is equivalent to an Euclidian Distance)
#'
#' @return This function return a distance matrix.
#'
#' @source Adapted from Mario Pineda-Krch (Great-circle distance calculations in R)
#'
#' @examples
#'
#' set.seed(1234)
#' x <- cbind(latitud = rnorm(5, -90), longitud = rnorm(5, 45))
#' dist_geographic(x[, 1], x[, 2], method = "harvesine")
#' @export

dist_geographic <- function(latitude, longitud, method = c(
                              "spherical",
                              "harvesine",
                              "manhattan",
                              "minkowski"
                            ),
                            places = NULL, dd_to_radians = FALSE, p = NULL) {
  if (!is.numeric(latitude)) stop("Vector is not numeric")
  if (!is.numeric(longitud)) stop("Vector is not numeric")

  profile <- list()
  profile2 <- list()
  R <- 6371 # Earth mean radius [km]

  # degree to radians
  if (dd_to_radians) {
    latitude <- latitude * pi / 180
    longitud <- longitud * pi / 180
  }

  method <- switch(sim_method(method),
    "spherical" = 1,
    "harvesine" = 2,
    "manhattan" = 3,
    "minkowski" = 4
  )

  if (method == 1) { # Spherical Law of Cosines
    for (i in 1:length(latitude)) {
      for (j in i:length(latitude)) {
        profile[[j]] <- suppressWarnings(acos(sin(latitude[i]) * sin(latitude[j]) + cos(latitude[i]) * cos(latitude[j]) * cos(longitud[j] - longitud[i])) * R)
      }
      profile2[[i]] <- unlist(profile)
    }
    m1 <- do.call(rbind, profile2)
    diag(m1) <- 0
    m1[lower.tri(m1)] <- t(m1)[lower.tri(m1)]
  }

  if (method == 2) { # Haversine formula
    for (i in 1:length(latitude)) {
      for (j in i:length(latitude)) {
        delta.long <- (longitud[j] - longitud[i])
        delta.lat <- (latitude[j] - latitude[i])
        a <- sin(delta.lat / 2)^2 + cos(latitude[i]) * cos(latitude[j]) * sin(delta.long / 2)^2
        c <- 2 * asin(min(1, sqrt(a)))
        profile[[j]] <- R * c
      }
      profile2[[i]] <- unlist(profile)
    }
    m1 <- do.call(rbind, profile2)
    diag(m1) <- 0
    m1[lower.tri(m1)] <- t(m1)[lower.tri(m1)]
  }

  if (method == 3) { # manhattan distance
    for (i in 1:length(latitude)) {
      for (j in i:length(latitude)) {
        profile[[j]] <- abs(latitude[i] - latitude[j]) + abs(longitud[i] - longitud[j])
      }
      profile2[[i]] <- unlist(profile)
    }
    m1 <- do.call(rbind, profile2)
    diag(m1) <- 0
    m1[lower.tri(m1)] <- t(m1)[lower.tri(m1)]
  }

  if (method == 4) { # minkowski distance

    if (is.null(p)) {
      p <- 2
    }

    for (i in 1:length(latitude)) {
      for (j in i:length(latitude)) {
        profile[[j]] <- (abs(latitude[i] - latitude[j]) + abs(longitud[i] - longitud[j])^p)^(1 / p)
      }
      profile2[[i]] <- unlist(profile)
    }
    m1 <- do.call(rbind, profile2)
    diag(m1) <- 0
    m1[lower.tri(m1)] <- t(m1)[lower.tri(m1)]
  }

  if (is.null(places)) {
    return(m1)
  } else {
    rownames(m1) <- places
    colnames(m1) <- places
    return(m1)
  }
}


#' Spatial autocorrelation
#'
#' This function calculate some spatial autocorrelations for a sample of networks at different orders (distances).
#'
#' @param A   A symmetric matrix
#' @param V   A vector
#' @param measures   Whether to use the Covariance \code{covariance} (default), Correlation \code{correlation}, Moran I \code{moran} or Geary's C \code{geary}
#' @param mean  Whether to use the mean of the vector for the measures
#' @param diag  Whether to consider the diagonal of the matrix for the measures
#' @param distance1  Whether to return only the spatial autocorrelation considering the actor at distance 1
#' @param rowstand  Whether to use the row-standardization to estimate Moran I (Anselin, 1995)
#' @param scale  Whether to scale Moran I (Anselin, 1995)
#'
#' @return This function return the global spatial autocorrelation. Multiple orders can also be computed.
#'
#' @references
#'
#' Anselin, L. (1995). Local indicators of spatial association—LISA. Geographical analysis, 27(2), 93-115.
#'
#' Geary, R.C. (1954). “The Contiguity Ratio and Statistical Mapping.” The Incorporated Statistician, 5: 115-145.
#'
#' Moran, P.A.P. (1950). “Notes on Continuous Stochastic Phenomena.” Biometrika, 37: 17-23.
#'
#' @importFrom stats sd var
#'
#' @examples
#' A <- matrix(c(
#'   0, 0, 1, 1,
#'   0, 0, 1, 0,
#'   1, 0, 0, 0,
#'   1, 0, 1, 0
#' ), byrow = TRUE, ncol = 4)
#' V <- c(2, 2, 1, 1)
#'
#' spatial_cor(A, V, measures = c("moran"))
#' @export

spatial_cor <- function(A, V, measures = c(
                          "covariance", "correlation",
                          "moran", "geary"
                        ),
                        mean = TRUE, diag = FALSE, distance1 = TRUE,
                        rowstand = FALSE, scale = FALSE) {
  A <- as.matrix(A)
  if (nrow(A) != length(V)) stop("The matrix must have the same number of rows as the vector")
  if (nrow(A) != ncol(A)) stop("The matrix should be square")

  measures <- switch(method(measures),
    "covariance" = 1,
    "correlation" = 2,
    "moran" = 3,
    "geary" = 4
  )
  if (!diag) {
    diag(A) <- 0
  }
  if (mean) {
    y <- V - mean(V)
  } else {
    y <- V
  }

  miss_vec <- is.na(V)
  if (any(miss_vec)) {
    V <- V[!miss_vec]
    A <- A[!miss_vec, !miss_vec]
    message("The vector has NA elements, and it will be removed in the vector and matrix")
  }
  if (any(is.na(A))) {
    A <- ifelse(is.na(A), 0, A)
    message("The matrix has NA elements. For the analysis, these elements are replaced with zeros.")
  }

  measure <- list()
  for (i in 1:nrow(A)) {
    weight <- ifelse(bfs_ugraph(A) == i - 1, 1, 0)
    if (measures == 1) { # covariance
      measure[[i]] <- (t(y) %*% weight %*% y) / sum(weight)
    }
    if (measures == 2) { # correlation
      measure[[i]] <- ((t(y) %*% weight %*% y) / sum(weight)) / var(y)
    }
    if (measures == 3) { # Moran I
      y <- V - mean(V)
      if (rowstand) {
        temp <- rowSums(weight)
        temp[temp == 0] <- 1
        weight <- weight / temp
      }
      measure[[i]] <- nrow(A) / sum(weight) * sum(weight * (y %o% y)) / sum(y^2)
      if (scale) {
        i.max <- (nrow(A) / sum(weight)) * (sd(rowSums(weight) * y) / sqrt(sum(y^2) / (nrow(A) - 1)))
        measure[[i]] <- measure[[i]] / i.max
      }
    }
    if (measures == 4) { # Geary C
      measure[[i]] <- (nrow(A) - 1) / (2 * sum(weight)) * sum(weight * outer(y, y, "-")^2) / sum((y - mean(y))^2)
    }
  }
  if (distance1) {
    return(unlist(measure)[2])
  } else {
    return(unlist(measure))
  }
}

method <- function(arg, choices, several.ok = FALSE) {
  if (missing(choices)) {
    formal.args <- formals(sys.function(sys.parent()))
    choices <- eval(formal.args[[deparse(substitute(arg))]])
  }

  arg <- tolower(arg)
  choices <- tolower(choices)

  match.arg(arg = arg, choices = choices, several.ok = several.ok)
}
