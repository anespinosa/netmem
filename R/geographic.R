#' Geographical distances
#'
#' This function calculate some geographical distances considering a list of places specifying their latitud and longitud. The function currently works for degree decimal or radians formats.
#'
#' @param latitute   A vector with latitute
#' @param longitud   A vector with longitud
#' @param method   Whether to use the Spherical Law of Cosines \code{spherical} (default) or Haversine formula \code{harvesine}
#' @param places   A vector with the names of the places
#' @param dd_to_radians  Whether to transform degree decimal format to radians
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

dist_geographic <- function(latitute, longitud, method = c(
                              "spherical",
                              "harvesine"
                            ),
                            places = NULL, dd_to_radians = FALSE) {
  if (!is.numeric(latitute)) stop("Vector is not numeric")
  if (!is.numeric(longitud)) stop("Vector is not numeric")

  profile <- list()
  profile2 <- list()
  R <- 6371 # Earth mean radius [km]

  # degree to radians
  if (dd_to_radians) {
    latitute <- latitute * pi / 180
    longitud <- longitud * pi / 180
  }

  method <- switch(sim_method(method),
    "spherical" = 1,
    "harvesine" = 2
  )

  if (method == 1) { # Spherical Law of Cosines
    for (i in 1:length(latitute)) {
      for (j in i:length(latitute)) {
        profile[[j]] <- suppressWarnings(acos(sin(latitute[i]) * sin(latitute[j]) + cos(latitute[i]) * cos(latitute[j]) * cos(longitud[j] - longitud[i])) * R)
      }
      profile2[[i]] <- unlist(profile)
    }
    m1 <- do.call(rbind, profile2)
    diag(m1) <- 0
    m1[lower.tri(m1)] <- t(m1)[lower.tri(m1)]
  }

  if (method == 2) { # Haversine formula
    for (i in 1:length(latitute)) {
      for (j in i:length(latitute)) {
        delta.long <- (longitud[j] - longitud[i])
        delta.lat <- (latitute[j] - latitute[i])
        a <- sin(delta.lat / 2)^2 + cos(latitute[i]) * cos(latitute[j]) * sin(delta.long / 2)^2
        c <- 2 * asin(min(1, sqrt(a)))
        profile[[j]] <- R * c
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
