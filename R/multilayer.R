#' Supra-adjacency matrix
#'
#' Arranges the layers of a multiplex network, in which the same actors are connected by several relations, into a
#' single matrix of actor-layer pairs (De Domenico et al., 2013; Kivela et al., 2014).
#'
#' @details
#' The supra-adjacency matrix has one row and one column for each actor in each layer. The blocks of the diagonal are
#' the layers, and the blocks outside it are the coupling between the layers, which joins the copies of the same actor.
#'
#' \code{coupling = "categorical"} (default) joins the copies of an actor in every pair of layers, which is the usual
#' choice when the layers are different relations. \code{"ordinal"} joins them only in consecutive layers, for layers
#' that follow an order, such as time. \code{"none"} leaves the layers apart, and the result is block diagonal.
#'
#' The \code{weight} of the coupling is the value given to those ties, and it sets how much the layers are held
#' together in measures computed on the whole structure.
#'
#' The rows and the columns are named after the actor and the layer, as \code{actor_layer}, taking the names of the
#' matrices and of the list of layers when they have them.
#'
#' @param layers   A list of square matrices of the same order, one for each layer, with the same actors in the same order
#' @param coupling   Whether the copies of an actor are joined in every pair of layers (\code{categorical}, default), only in consecutive layers (\code{ordinal}), or not at all (\code{none})
#' @param weight   The value of the ties between the copies of the same actor
#' @param sparse   Whether to return a sparse matrix of the \code{Matrix} package
#'
#' @return This function returns the supra-adjacency matrix of the layers.
#'
#' @references
#'
#' De Domenico, M., Sole-Ribalta, A., Cozzo, E., Kivela, M., Moreno, Y., Porter, M. A., Gomez, S. and Arenas, A. (2013). Mathematical formulation of multilayer networks. Physical Review X, 3(4), 041022. \doi{10.1103/PhysRevX.3.041022}
#'
#' Kivela, M., Arenas, A., Barthelemy, M., Gleeson, J. P., Moreno, Y. and Porter, M. A. (2014). Multilayer networks. Journal of Complex Networks, 2(3), 203-271. \doi{10.1093/comnet/cnu016}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A1 <- matrix(c(
#'   0, 1, 0,
#'   1, 0, 1,
#'   0, 1, 0
#' ), byrow = TRUE, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))
#'
#' A2 <- matrix(c(
#'   0, 0, 1,
#'   0, 0, 0,
#'   1, 0, 0
#' ), byrow = TRUE, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))
#'
#' supra_adjacency(list(advice = A1, friendship = A2))
#'
#' # Layers that follow an order are coupled only with the next one
#' supra_adjacency(list(A1, A2), coupling = "ordinal")
#' @export

supra_adjacency <- function(layers, coupling = c("categorical", "ordinal", "none"),
                            weight = 1, sparse = FALSE) {
  coupling <- match.arg(coupling)
  layers <- check_layers(layers)

  L <- length(layers)
  n <- nrow(layers[[1]])
  actors <- rownames(layers[[1]])
  if (is.null(actors)) actors <- paste0("n", seq_len(n))
  names_layers <- names(layers)
  if (is.null(names_layers)) names_layers <- paste0("L", seq_len(L))

  S <- matrix(0, nrow = n * L, ncol = n * L)
  labels <- character(n * L)
  for (k in seq_len(L)) {
    rows <- ((k - 1) * n + 1):(k * n)
    S[rows, rows] <- layers[[k]]
    labels[rows] <- paste(actors, names_layers[k], sep = "_")
  }

  # The copies of an actor are joined by the coupling. With an order, only the
  # layers that follow each other are joined
  if (coupling != "none" && L > 1) {
    for (k in seq_len(L - 1)) {
      others <- if (coupling == "ordinal") k + 1 else (k + 1):L
      for (k2 in others) {
        rows <- ((k - 1) * n + 1):(k * n)
        cols <- ((k2 - 1) * n + 1):(k2 * n)
        S[cbind(rows, cols)] <- weight
        S[cbind(cols, rows)] <- weight
      }
    }
  }

  rownames(S) <- labels
  colnames(S) <- labels
  if (sparse) S <- Matrix::Matrix(S, sparse = TRUE)
  # The actors and the layers are kept, so that aggregate_layers() can name the
  # result after the actors and count the layers
  attr(S, "actors") <- actors
  attr(S, "layers") <- names_layers
  S
}

#' Aggregation of the layers
#'
#' Reduces a multiplex network to a single matrix, in which the actors are joined when they are connected in the
#' layers (Battiston et al., 2014; De Domenico et al., 2015).
#'
#' @details
#' \code{method = "sum"} (default) adds the layers, so the value of a tie is the number of layers in which it is
#' present, which Battiston et al. (2014) call the overlapping network. \code{"binary"} gives one to every tie that is
#' present in at least one layer, and \code{"mean"} divides the sum by the number of layers.
#'
#' The layers are given as a list of matrices, or as the supra-adjacency matrix of \code{supra_adjacency()} together
#' with the number of layers \code{l}, in which case the coupling between the layers is ignored.
#'
#' Aggregating loses the information of which layer each tie belongs to, and measures computed on the aggregated
#' network can differ from the ones computed on the layers (De Domenico et al., 2015).
#'
#' @param layers   A list of square matrices of the same order, or a supra-adjacency matrix with \code{l} layers
#' @param method   Whether the ties are added (\code{sum}, default), made binary (\code{binary}) or averaged (\code{mean})
#' @param l   The number of layers, when \code{layers} is a supra-adjacency matrix that does not come from \code{supra_adjacency()}
#'
#' @return This function returns a square matrix with the actors of the layers.
#'
#' @references
#'
#' Battiston, F., Nicosia, V. and Latora, V. (2014). Structural measures for multiplex networks. Physical Review E, 89(3), 032804. \doi{10.1103/PhysRevE.89.032804}
#'
#' De Domenico, M., Nicosia, V., Arenas, A. and Latora, V. (2015). Structural reducibility of multilayer networks. Nature Communications, 6, 6864. \doi{10.1038/ncomms7864}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' A1 <- matrix(c(
#'   0, 1, 0,
#'   1, 0, 1,
#'   0, 1, 0
#' ), byrow = TRUE, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))
#'
#' A2 <- matrix(c(
#'   0, 1, 1,
#'   1, 0, 0,
#'   1, 0, 0
#' ), byrow = TRUE, ncol = 3, dimnames = list(letters[1:3], letters[1:3]))
#'
#' # The tie a-b is in both layers
#' aggregate_layers(list(A1, A2))
#' aggregate_layers(list(A1, A2), method = "binary")
#'
#' # The same from the supra-adjacency matrix, which knows its layers
#' aggregate_layers(supra_adjacency(list(A1, A2)))
#' @export

aggregate_layers <- function(layers, method = c("sum", "binary", "mean"), l = NULL) {
  method <- match.arg(method)

  if (!is.list(layers)) {
    # SUPRA-ADJACENCY: the layers are the blocks of the diagonal
    actors <- attr(layers, "actors")
    if (is.null(l)) l <- length(attr(layers, "layers"))
    if (is.null(l) || l == 0) l <- NULL
    S <- as.matrix(layers)
    if (is.null(l)) stop("Give the number of layers of the supra-adjacency matrix in `l`")
    if (nrow(S) != ncol(S)) stop("Matrix should be square")
    if (nrow(S) %% l != 0) stop("The number of rows is not a multiple of the number of layers")
    n <- nrow(S) / l
    blocks <- vector("list", l)
    for (k in seq_len(l)) {
      rows <- ((k - 1) * n + 1):(k * n)
      block <- S[rows, rows, drop = FALSE]
      if (!is.null(actors)) dimnames(block) <- list(actors, actors)
      blocks[[k]] <- block
    }
    layers <- blocks
  }
  layers <- check_layers(layers)

  A <- layers[[1]]
  if (length(layers) > 1) {
    for (k in 2:length(layers)) {
      A <- A + layers[[k]]
    }
  }

  if (method == "binary") A <- ifelse(A > 0, 1, 0)
  if (method == "mean") A <- A / length(layers)
  A
}

# The layers of a multiplex network are square matrices of the same order, with
# the actors in the same order. Missing values are treated as absent ties, as in
# the rest of the package
check_layers <- function(layers) {
  if (!is.list(layers)) stop("The layers should be given in a list of matrices")
  if (length(layers) == 0) stop("The list of layers is empty")

  layers <- lapply(layers, as.matrix)
  if (any(vapply(layers, function(x) nrow(x) != ncol(x), logical(1)))) {
    stop("Every layer should be a square matrix")
  }
  orders <- vapply(layers, nrow, numeric(1))
  if (length(unique(orders)) > 1) {
    stop("Every layer should have the same actors, in the same order")
  }
  lapply(layers, function(x) {
    if (any(is.na(x) == TRUE)) x <- ifelse(is.na(x), 0, x)
    x
  })
}
