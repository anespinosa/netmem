# TODO: Add NA for all censuses

#' Dyad census
#'
#' @param G   A symmetric matrix object.
#' @param directed   Whether the matrix is directed or not
#' @param loops   Whether to expect nonzero elements in the diagonal of the matrix
#'
#' @return This function return the counts of the dyad census.
#'
#' @references
#'
#' Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and applications. Cambridge University Press.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' data(krackhardt_friends)
#' dyadic_census(krackhardt_friends)
#'
#' data(FIFAin)
#' dyadic_census(FIFAin[[1]], directed = FALSE)
#' @export

dyadic_census <- function(G, directed = TRUE, loops = FALSE) {
  G <- as.matrix(G)
  A <- G

  if (!loops) {
    diag(G) <- 0
  }

  if (any(abs(G > 1), na.rm = TRUE)) stop("The matrix should be binary")
  g <- nrow(G)
  na <- sum(is.na(G)) - sum(diag(is.na(G)))

  if (any(is.na(G) == TRUE)) {
    G <- ifelse(is.na(G), 0, G)
  }

  m <- (1 / 2) * sum(diag(G %*% G))
  a <- sum(diag(G %*% t(G))) - sum(diag(G %*% G))
  n <- ((g * (g - 1)) / 2) - (sum(diag(G %*% t(G))) - sum(diag(G %*% G))) - ((1 / 2) * sum(diag(G %*% G)))

  if (directed) {
    if (any(is.na(A) == TRUE)) {
      c(
        "Mutual" = m,
        "Asymmetrics" = a,
        "Nulls" = n - na,
        "NA" = na
      )
    } else {
      c(
        "Mutual" = m,
        "Asymmetrics" = a,
        "Nulls" = n
      )
    }
  } else {
    if (any(is.na(A) == TRUE)) {
      c(
        "Mutual" = m,
        "Nulls" = n - na,
        "NA" = na
      )
    } else {
      c(
        "Mutual" = m,
        "Nulls" = n
      )
    }
  }
}

#' Multiplex triad census
#'
#' This function counts the different subgraphs of three nodes in a multiplex directed and undirected network.
#'
#' @details
#' Each triple of nodes is classified by its type in the first (directed) network, one of the 16 types of the
#' triad census (Holland and Leinhardt, 1976), and by the position of the edges of the second (undirected) network
#' in that triad (Espinosa-Rada, 2021: Figure 12). Each type of the first network is drawn in fixed positions,
#' bottom left, top and bottom right, and the edges of the second network are named by where they fall: 102a
#' (bottom left to top), 102b (bottom left to bottom right) and 102c (top to bottom right); the two-paths by their
#' centre, 201a (bottom left), 201b (bottom right) and 201c (top). Positions that are equivalent by the symmetry
#' of the triad of the first network form a single class, such as \code{021U_102ac}, as the two edges between the
#' top and the bottom nodes are equivalent when both bottom nodes send a tie to the top one.
#'
#' With \code{merge = "overlap"}, the classes of the same type of the first network that give the same triad when
#' both networks are overlapped are also merged, as most groups of Figure 12 do (for instance, \code{102_003-102a}:
#' an edge of the second network on a mutual tie of the first adds nothing to the overlapped triad).
#'
#' The counts of each type of the first network add up to its triad census.
#'
#' Up to version 1.0-3 the function added counts of the two networks instead of counting the triples of each
#' class, so its results were wrong.
#'
#' @param A   A directed matrix object.
#' @param B   An undirected matrix object. A directed matrix is replaced by its underlying graph.
#' @param merge   Whether to merge the classes that give the same overlapped triad (\code{overlap}) or not (\code{none}, default).
#'
#' @return This function gives the number of triples in each class, named by the type of the first network and the position of the edges of the second.
#'
#' @references
#'
#' Batagelj, V. and Mrvar, A. (2001). A subquadratic triad census algorithm for large sparse networks with small maximum degree. Social Networks, 23(3), 237–243. \doi{10.1016/S0378-8733(01)00035-1}
#'
#' Espinosa-Rada, A. (2021). A Network Approach for the Sociological Study of Science: Modelling Dynamic Multilevel Networks. [PhD](https://research.manchester.ac.uk/en/studentTheses/a-network-approach-for-the-sociological-study-of-science-and-know). The University of Manchester.
#'
#' Espinosa-Rada, A., Bellotti, E., Everett, M., & Stadtfeld, C. (2024). Co-evolution of a socio-cognitive scientific network: A case study of citation dynamics among astronomers. Social Networks, 78, 92–108. \doi{10.1016/j.socnet.2023.11.008}
#'
#' Holland, P. W. and Leinhardt, S. (1976). Local structure in social networks. Sociological Methodology, 7, 1–45. \doi{10.2307/270703}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' # SOAR
#' A <- matrix(
#'   c(
#'     0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1,
#'     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
#'     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'     0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1,
#'     0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0,
#'     0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'     0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1,
#'     0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
#'     0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1,
#'     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#'   ),
#'   byrow = TRUE, ncol = 12
#' )
#'
#' B <- matrix(
#'   c(
#'     0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1,
#'     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
#'     1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0,
#'     0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'     1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#'     0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,
#'     0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
#'   ),
#'   byrow = TRUE, ncol = 12
#' )
#'
#' multiplex_census(A, B)
#' @export
#'

multiplex_census <- function(A, B, merge = c("none", "overlap")) {
  merge <- match.arg(merge)
  A <- as.matrix(A)
  B <- as.matrix(B)
  A <- ifelse(is.na(A), 0, A)
  B <- ifelse(is.na(B), 0, B)
  if (!dim(A)[1] == dim(A)[2]) stop("Matrix should be square")
  if (!dim(B)[1] == dim(B)[2]) stop("Matrix should be square")
  if (nrow(A) != nrow(B)) stop("Both networks should have the same nodes")
  if (nrow(A) < 3) stop("The triad census needs at least three nodes")
  if (!all(A <= 1)) warning("Measure only implemented for binary networks, the first network is binarized")
  if (!all(B <= 1)) warning("Measure only implemented for binary networks, the second network is binarized")
  A <- ifelse(A > 0, 1, 0)
  B <- ifelse(B > 0, 1, 0)
  diag(A) <- 0
  diag(B) <- 0
  if (!all(B == t(B))) warning("The second network is directed, the underlying graph is used")
  B <- pmax(B, t(B))

  # The class of each of the 64 x 8 combinations of the six possible arcs of
  # the first network and the three possible edges of the second
  table <- multiplex_classes(merge)

  triples <- t(utils::combn(nrow(A), 3))
  i <- triples[, 1]
  j <- triples[, 2]
  k <- triples[, 3]
  code_a <- A[cbind(i, j)] + 2 * A[cbind(j, i)] + 4 * A[cbind(i, k)] +
    8 * A[cbind(k, i)] + 16 * A[cbind(j, k)] + 32 * A[cbind(k, j)]
  code_b <- B[cbind(i, j)] + 2 * B[cbind(i, k)] + 4 * B[cbind(j, k)]
  class <- table$class[code_a * 8 + code_b + 1]

  census <- table(factor(class, levels = unique(table$class[order(table$order)])))
  census <- setNames(as.numeric(census), names(census))
  return(census)
}

# Classes of the mixed multiplex triad census (Espinosa-Rada, 2021: Figure 12).
# Each type of triad of the first network is drawn in fixed positions: 1 at the
# bottom left, 2 at the top and 3 at the bottom right. The edges of the second
# network are named by their position: 102a (1-2), 102b (1-3), 102c (2-3), and
# the two-paths by their centre: 201a (1), 201b (3), 201c (2). Positions that
# the symmetries of the first triad make equivalent are merged (e.g. 102ac).
# With merge = "overlap", the classes of a type of the first network that give
# the same triad when both networks are overlapped are also merged.
multiplex_classes <- function(merge = "none") {
  drawings <- list(
    "003" = NULL,
    "012" = rbind(c(1, 2)),
    "102" = rbind(c(1, 2), c(2, 1)),
    "021D" = rbind(c(2, 1), c(2, 3)),
    "021U" = rbind(c(1, 2), c(3, 2)),
    "021C" = rbind(c(1, 2), c(2, 3)),
    "111D" = rbind(c(1, 3), c(3, 1), c(2, 3)),
    "111U" = rbind(c(1, 3), c(3, 1), c(3, 2)),
    "030T" = rbind(c(1, 2), c(3, 2), c(1, 3)),
    "030C" = rbind(c(2, 1), c(1, 3), c(3, 2)),
    "201" = rbind(c(1, 2), c(2, 1), c(1, 3), c(3, 1)),
    "120D" = rbind(c(2, 1), c(2, 3), c(1, 3), c(3, 1)),
    "120U" = rbind(c(1, 2), c(3, 2), c(1, 3), c(3, 1)),
    "120C" = rbind(c(1, 2), c(2, 3), c(1, 3), c(3, 1)),
    "210" = rbind(c(1, 2), c(2, 3), c(3, 2), c(1, 3), c(3, 1)),
    "300" = rbind(c(1, 2), c(2, 1), c(1, 3), c(3, 1), c(2, 3), c(3, 2))
  )
  types <- names(drawings)
  perms <- rbind(c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), c(2, 3, 1), c(3, 1, 2), c(3, 2, 1))
  drawn <- lapply(drawings, function(arcs) {
    M <- matrix(0, 3, 3)
    if (!is.null(arcs)) M[arcs] <- 1
    M
  })

  # Name of the edges of the second network in the drawn positions
  red_label <- function(E) {
    e <- c(E[1, 2], E[1, 3], E[2, 3])
    if (sum(e) == 0) return("003")
    if (sum(e) == 3) return("300")
    if (sum(e) == 1) return(paste0("102", c("a", "b", "c")[e == 1]))
    # The centre of a two-path is the node opposite the missing edge
    paste0("201", c("b", "c", "a")[e == 0])
  }

  classes <- NULL
  for (code_a in 0:63) {
    arcs <- as.integer(intToBits(code_a))[1:6]
    A <- matrix(0, 3, 3)
    A[1, 2] <- arcs[1]
    A[2, 1] <- arcs[2]
    A[1, 3] <- arcs[3]
    A[3, 1] <- arcs[4]
    A[2, 3] <- arcs[5]
    A[3, 2] <- arcs[6]
    # The type of the triad, and the ways of placing its nodes in the drawing
    type <- NULL
    placements <- NULL
    for (t in types) {
      for (p in seq_len(nrow(perms))) {
        if (all(A[perms[p, ], perms[p, ]] == drawn[[t]])) {
          type <- t
          placements <- c(placements, p)
        }
      }
      if (!is.null(type)) break
    }
    for (code_b in 0:7) {
      edges <- as.integer(intToBits(code_b))[1:3]
      E <- matrix(0, 3, 3)
      E[1, 2] <- E[2, 1] <- edges[1]
      E[1, 3] <- E[3, 1] <- edges[2]
      E[2, 3] <- E[3, 2] <- edges[3]
      orbit <- sort(unique(sapply(placements, function(p) red_label(E[perms[p, ], perms[p, ]]))))
      stem <- unique(substr(orbit, 1, 3))
      positions <- paste(substring(orbit, 4), collapse = "")
      red <- if (stem %in% c("003", "300") || nchar(positions) == 3) stem else paste0(stem, positions)
      # Type of the triad when both networks are overlapped
      O <- pmax(A, E)
      code_o <- O[1, 2] + 2 * O[2, 1] + 4 * O[1, 3] + 8 * O[3, 1] + 16 * O[2, 3] + 32 * O[3, 2]
      classes <- rbind(classes, data.frame(
        code = code_a * 8 + code_b, first = type, second = red,
        overlap = code_o, stringsAsFactors = FALSE
      ))
    }
  }
  overlap_type <- sapply(classes$overlap, function(code) {
    A <- matrix(0, 3, 3)
    arcs <- as.integer(intToBits(code))[1:6]
    A[1, 2] <- arcs[1]
    A[2, 1] <- arcs[2]
    A[1, 3] <- arcs[3]
    A[3, 1] <- arcs[4]
    A[2, 3] <- arcs[5]
    A[3, 2] <- arcs[6]
    for (t in types) {
      for (p in seq_len(nrow(perms))) {
        if (all(A[perms[p, ], perms[p, ]] == drawn[[t]])) return(t)
      }
    }
  })
  classes$overlap <- overlap_type
  classes$class <- paste(classes$first, classes$second, sep = "_")
  if (merge == "overlap") {
    for (t in types) {
      for (o in unique(classes$overlap[classes$first == t])) {
        members <- classes$first == t & classes$overlap == o
        seconds <- unique(classes$second[members][order(classes$code[members])])
        classes$class[members] <- paste(t, paste(sort(seconds), collapse = "-"), sep = "_")
      }
    }
  }
  # The classes are listed by the type of the first network, then by the
  # number of edges of the second
  edges_second <- as.numeric(substr(classes$second, 1, 1))
  classes$order <- match(classes$first, types) * 100 + edges_second * 10 + nchar(classes$second)
  return(classes[order(classes$code), ])
}

#' Multilevel triad and quadrilateral census
#'
#' @param A1   An adjacent matrix object.
#' @param B1   An incidence matrix object.
#' @param B2   An incidence matrix object.
#' @param quad  Whether the matrix is a quadrilateral census or not.
#'
#' @return This function return the counts of a multilevel census.
#'
#' If \code{quad = TRUE}, then the function return the multilevel quadrilateral census.
#'
#' @references
#'
#' Espinosa-Rada, A. (2021). A Network Approach for the Sociological Study of Science: Modelling Dynamic Multilevel Networks. [PhD](https://research.manchester.ac.uk/en/studentTheses/a-network-approach-for-the-sociological-study-of-science-and-know). The University of Manchester.
#'
#' Espinosa-Rada, A., Bellotti, E., Everett, M., & Stadtfeld, C. (2024). Co-evolution of a socio-cognitive scientific network: A case study of citation dynamics among astronomers. Social Networks, 78, 92–108. \doi{10.1016/j.socnet.2023.11.008}
#'
#' Hollway, J., Lomi, A., Pallotti, F., & Stadtfeld, C. (2017). Multilevel social spaces: The network dynamics of organizational fields. Network Science, 5(2), 187–212. \doi{10.1017/nws.2017.8}
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#'
#' B1 <- matrix(c(
#'   1, 1, 0,
#'   0, 0, 1,
#'   0, 0, 1,
#'   1, 0, 0
#' ), byrow = TRUE, ncol = 3)
#' A1 <- matrix(c(
#'   0, 1, 0, 1,
#'   1, 0, 0, 1,
#'   0, 1, 0, 1,
#'   1, 0, 1, 0
#' ), byrow = TRUE, ncol = 4)
#' B2 <- matrix(c(
#'   1, 0, 0, 0, 0,
#'   0, 1, 0, 1, 0,
#'   0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0
#' ), byrow = TRUE, ncol = 5)
#'
#' mixed_census(A1, B1, B2, quad = TRUE)
#' @export
#'

mixed_census <- function(A1, B1, B2 = NULL, quad = FALSE) {
  if (!is.null(B2) & quad == FALSE) {
    stop("For the quadrilateral census you should specify `quad=TRUE` in the function")
  }
  if (dim(A1)[1] != dim(A1)[2]) stop("Matrix is not square")
  if (dim(B1)[1] == dim(B1)[2]) warning("Matrix should be rectangular")
  if (!dim(A1)[1] == dim(B1)[1]) stop("Non-conformable arrays")

  if (any(is.na(A1) == TRUE)) {
    A1 <- ifelse(is.na(A1), 0, A1)
  }
  if (any(is.na(B1) == TRUE)) {
    B1 <- ifelse(is.na(B1), 0, B1)
  }
  if (any(is.na(B2) == TRUE)) {
    B2 <- ifelse(is.na(B2), 0, B2)
  }

  if (any(abs(A1 > 1), na.rm = TRUE)) stop("The matrix should be binary")
  if (any(abs(B1 > 1), na.rm = TRUE)) stop("The matrix should be binary")
  if (any(abs(B2 > 1), na.rm = TRUE)) stop("The matrix should be binary")

  m1 <- as.matrix(A1)
  m2 <- as.matrix(B1)
  cp <- function(m) (-m + 1)

  onemode.reciprocal <- m1 * t(m1)
  onemode.forward <- m1 * cp(t(m1))
  onemode.backward <- cp(m1) * t(m1)
  onemode.null <- cp(m1) * cp(t(m1))
  diag(onemode.forward) <- 0
  diag(onemode.backward) <- 0
  diag(onemode.null) <- 0

  bipartite.twopath <- m2 %*% t(m2)
  bipartite.null <- cp(m2) %*% cp(t(m2))
  bipartite.onestep1 <- m2 %*% cp(t(m2))
  bipartite.onestep2 <- cp(m2) %*% t(m2)
  diag(bipartite.twopath) <- 0
  diag(bipartite.null) <- 0
  diag(bipartite.onestep1) <- 0
  diag(bipartite.onestep2) <- 0

  res <- c(
    "22" = sum(onemode.reciprocal * bipartite.twopath) / 2,
    "21" = sum(onemode.forward * bipartite.twopath) / 2 + sum(onemode.backward * bipartite.twopath) / 2,
    "20" = sum(onemode.null * bipartite.twopath) / 2,
    "12" = sum(onemode.reciprocal * bipartite.onestep1) / 2 + sum(onemode.reciprocal * bipartite.onestep2) / 2,
    "11D" = sum(onemode.forward * bipartite.onestep1) / 2 + sum(onemode.backward * bipartite.onestep2) / 2,
    "11U" = sum(onemode.forward * bipartite.onestep2) / 2 + sum(onemode.backward * bipartite.onestep1) / 2,
    "10" = sum(onemode.null * bipartite.onestep2) / 2 + sum(onemode.null * bipartite.onestep1) / 2,
    "02" = sum(onemode.reciprocal * bipartite.null) / 2,
    "01" = sum(onemode.forward * bipartite.null) / 2 + sum(onemode.backward * bipartite.null) / 2,
    "00" = sum(onemode.null * bipartite.null) / 2
  )

  if (quad) {
    if (!dim(B2)[1] == dim(A1)[1]) stop("Non-conformable arrays")
    if (dim(B2)[1] == dim(B2)[2]) warning("Matrix should be rectangular")
    m3 <- as.matrix(B2)
    bipartite.twopath2 <- m3 %*% t(m3)
    bipartite.null2 <- cp(m3) %*% cp(t(m3))
    bipartite.onestep12 <- m3 %*% cp(t(m3))
    bipartite.onestep22 <- cp(m3) %*% t(m3)
    diag(bipartite.twopath2) <- 0
    diag(bipartite.null2) <- 0
    diag(bipartite.onestep12) <- 0
    diag(bipartite.onestep22) <- 0

    res <- c(
      "000" = sum(onemode.null * bipartite.null * bipartite.null2) / 2,
      "100" = sum(onemode.null * bipartite.onestep1 * bipartite.null2) / 2 + sum(onemode.null * bipartite.onestep2 * bipartite.null2) / 2,
      "001" = sum(onemode.null * bipartite.null * bipartite.onestep12) / 2 + sum(onemode.null * bipartite.null * bipartite.onestep22) / 2,
      "010" = sum(onemode.forward * bipartite.null * bipartite.null2) / 2 + sum(onemode.backward * bipartite.null * bipartite.null2) / 2,
      "020" = sum(onemode.reciprocal * bipartite.null * bipartite.null2) / 2,
      "200" = sum(onemode.null * bipartite.twopath * bipartite.null2) / 2,
      "11D0" = sum(onemode.forward * bipartite.onestep1 * bipartite.null2) / 2 + sum(onemode.backward * bipartite.onestep2 * bipartite.null2) / 2,
      "11U0" = sum(onemode.forward * bipartite.onestep2 * bipartite.null2) / 2 + sum(onemode.backward * bipartite.onestep1 * bipartite.null2) / 2,
      "120" = sum(onemode.reciprocal * bipartite.onestep1 * bipartite.null2) / 2 + sum(onemode.reciprocal * bipartite.onestep2 * bipartite.null2) / 2,
      "210" = sum(onemode.forward * bipartite.twopath * bipartite.null2) / 2 + sum(onemode.backward * bipartite.twopath * bipartite.null2) / 2,
      "220" = sum(onemode.reciprocal * bipartite.twopath * bipartite.null2) / 2,
      "002" = sum(onemode.null * bipartite.null * bipartite.twopath2) / 2,
      "01D1" = sum(onemode.forward * bipartite.null * bipartite.onestep22) / 2 + sum(onemode.backward * bipartite.null * bipartite.onestep12) / 2,
      "01U1" = sum(onemode.forward * bipartite.null * bipartite.onestep12) / 2 + sum(onemode.backward * bipartite.null * bipartite.onestep22) / 2,
      "012" = sum(onemode.forward * bipartite.null * bipartite.twopath2) / 2 + sum(onemode.backward * bipartite.null * bipartite.twopath2) / 2,
      "021" = sum(onemode.reciprocal * bipartite.null * bipartite.onestep12) / 2 + sum(onemode.reciprocal * bipartite.null * bipartite.onestep22) / 2,
      "022" = sum(onemode.reciprocal * bipartite.null * bipartite.twopath2) / 2,
      "101N" = sum(onemode.null * bipartite.onestep1 * bipartite.onestep22) / 2 + sum(onemode.null * bipartite.onestep2 * bipartite.onestep12) / 2,
      "101P" = sum(onemode.null * bipartite.onestep1 * bipartite.onestep12) / 2 + sum(onemode.null * bipartite.onestep2 * bipartite.onestep22) / 2,
      "201" = sum(onemode.null * bipartite.twopath * bipartite.onestep12) / 2 + sum(onemode.null * bipartite.twopath * bipartite.onestep22) / 2,
      "102" = sum(onemode.null * bipartite.onestep1 * bipartite.twopath2) / 2 + sum(onemode.null * bipartite.onestep2 * bipartite.twopath2) / 2,
      "202" = sum(onemode.null * bipartite.twopath * bipartite.twopath2) / 2,
      "11D1W" = sum(onemode.forward * bipartite.onestep1 * bipartite.onestep12) / 2 + sum(onemode.backward * bipartite.onestep2 * bipartite.onestep22) / 2,
      "11U1P" = sum(onemode.forward * bipartite.onestep2 * bipartite.onestep22) / 2 + sum(onemode.backward * bipartite.onestep1 * bipartite.onestep12) / 2,
      "11D1P" = sum(onemode.forward * bipartite.onestep1 * bipartite.onestep22) / 2 + sum(onemode.backward * bipartite.onestep2 * bipartite.onestep12) / 2,
      "11U1W" = sum(onemode.forward * bipartite.onestep2 * bipartite.onestep12) / 2 + sum(onemode.backward * bipartite.onestep1 * bipartite.onestep22) / 2,
      "121W" = sum(onemode.reciprocal * bipartite.onestep1 * bipartite.onestep12) / 2 + sum(onemode.reciprocal * bipartite.onestep2 * bipartite.onestep22) / 2,
      "121P" = sum(onemode.reciprocal * bipartite.onestep1 * bipartite.onestep22) / 2 + sum(onemode.reciprocal * bipartite.onestep2 * bipartite.onestep12) / 2,
      "21D1" = sum(onemode.forward * bipartite.twopath * bipartite.onestep12) / 2 + sum(onemode.backward * bipartite.twopath * bipartite.onestep22) / 2,
      "21U1" = sum(onemode.forward * bipartite.twopath * bipartite.onestep22) / 2 + sum(onemode.backward * bipartite.twopath * bipartite.onestep12) / 2,
      "11D2" = sum(onemode.forward * bipartite.onestep1 * bipartite.twopath2) / 2 + sum(onemode.backward * bipartite.onestep2 * bipartite.twopath2) / 2,
      "11U2" = sum(onemode.forward * bipartite.onestep2 * bipartite.twopath2) / 2 + sum(onemode.backward * bipartite.onestep1 * bipartite.twopath2) / 2,
      "221" = sum(onemode.reciprocal * bipartite.twopath * bipartite.onestep12) / 2 + sum(onemode.reciprocal * bipartite.twopath * bipartite.onestep22) / 2,
      "122" = sum(onemode.reciprocal * bipartite.onestep1 * bipartite.twopath2) / 2 + sum(onemode.reciprocal * bipartite.onestep2 * bipartite.twopath2) / 2,
      "212" = sum(onemode.forward * bipartite.twopath * bipartite.twopath2) / 2 + sum(onemode.backward * bipartite.twopath * bipartite.twopath2) / 2,
      "222" = sum(onemode.reciprocal * bipartite.twopath * bipartite.twopath2) / 2
    )
  }
  return(res)
}
