#' DAG validation and topological ordering for citation networks
#'
#' \code{dag_check} verifies whether a directed adjacency matrix represents a
#' directed acyclic graph (DAG) and, when it does not, removes a set of arcs
#' that breaks every cycle.  \code{dag_sort} returns the node names in
#' topological order (sources first, sinks last).
#'
#' In the convention used throughout the main path functions, \code{A[i,j] > 0}
#' means that knowledge flows from paper \code{i} to paper \code{j}, that is,
#' paper \code{j} cites paper \code{i} (Liu and Lu, 2012; Kuan, 2020). Sources
#' (in-degree zero) are the papers that cite nobody in the corpus; sinks
#' (out-degree zero) are the papers that nobody in the corpus cites. A matrix
#' in which \code{A[i,j] > 0} means that \code{i} cites \code{j} should be
#' transposed first with \code{t(A)}.
#'
#' The order is obtained with the algorithm of Kahn (1962): the sources are
#' placed first, their arcs are removed, and the nodes left without incoming
#' arcs are placed next, until no node remains. When some nodes are never left
#' without incoming arcs, they lie on a cycle.
#'
#' A citation network can have cycles, for instance when two papers published
#' at the same time cite each other, and a paper citing itself is a cycle of
#' length one. \code{dag_check} removes the self-citations and then the arcs
#' that point backwards in the ordering of Eades, Lin and Smyth (1993). Arcs that
#' can be restored without closing a cycle are restored, so that no removed arc
#' is unnecessary. The set is small but is not guaranteed to be the smallest
#' possible, which is a hard problem. Liu, Lu and Ho (2019) discuss alternatives
#' that keep all the citations, such as merging the papers of a cycle into one
#' node, which should be applied before this function when the removed arcs
#' matter.
#'
#' @name dag
#'
#' @param A   A square, named, directed adjacency matrix.
#'
#' @return
#' \code{dag_check} returns a named list:
#' \describe{
#'   \item{\code{is_dag}}{Logical; \code{TRUE} if the original matrix was already acyclic.}
#'   \item{\code{n_removed}}{Integer number of arcs removed, self-citations included.}
#'   \item{\code{A}}{Adjacency matrix with the removed arcs set to zero.}
#' }
#' \code{dag_sort} returns a character vector of node names in topological order.
#'
#' @references
#'
#' Eades, P., Lin, X. and Smyth, W.F. (1993). A fast and effective heuristic for
#' the feedback arc set problem. Information Processing Letters. 47(6): 319-323.
#' \doi{10.1016/0020-0190(93)90079-O}.
#'
#' Hummon, N.P. and Doreian, P. (1989). Connectivity in a citation network:
#' The development of DNA theory. Social Networks. 11(1): 39-63.
#' \doi{10.1016/0378-8733(89)90017-8}.
#'
#' Kahn, A.B. (1962). Topological sorting of large networks. Communications of
#' the ACM. 5(11): 558-562. \doi{10.1145/368996.369025}.
#'
#' Liu, J.S., Lu, L.Y.Y. and Ho, M.H.C. (2019). A few notes on main path
#' analysis. Scientometrics. 119(1): 379-391. \doi{10.1007/s11192-019-03034-x}.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' # P1 is cited by P2 and P3, which are both cited by P4
#' A <- matrix(c(
#'   0, 1, 1, 0,
#'   0, 0, 0, 1,
#'   0, 0, 0, 1,
#'   0, 0, 0, 0
#' ), byrow = TRUE, nrow = 4)
#' rownames(A) <- c("P1", "P2", "P3", "P4")
#' colnames(A) <- c("P1", "P2", "P3", "P4")
#'
#' dag_check(A)
#'
#' # P4 is also cited by P1, which closes a cycle
#' A_cycle <- A
#' A_cycle["P4", "P1"] <- 1
#' dag_check(A_cycle)
#' @importFrom stats setNames
#' @export

dag_check <- function(A) {
  A <- as.matrix(A)
  if (is.null(rownames(A)) || is.null(colnames(A))) stop("A must have named rows and columns")
  if (nrow(A) != ncol(A)) stop("A must be a square matrix")
  if (!all(rownames(A) == colnames(A))) stop("Row and column names must match")

  n_loops <- sum(diag(A) > 0)
  diag(A) <- 0
  B <- 1 * (A > 0)
  B[is.na(B)] <- 0

  if (!is.null(.topo_order(B))) {
    if (n_loops == 0) {
      return(list(is_dag = TRUE, n_removed = 0L, A = A))
    }
    warning(sprintf("%d arc(s) removed to enforce the DAG structure", n_loops))
    return(list(is_dag = FALSE, n_removed = as.integer(n_loops), A = A))
  }

  # Ordering of Eades, Lin and Smyth (1993). The sinks are placed at the end and
  # the sources at the beginning, as they cannot be on a cycle. When neither is
  # left, the node whose outgoing arcs most exceed its incoming arcs is placed
  # next at the beginning, so that few arcs point backwards
  n <- nrow(B)
  remaining <- rep(TRUE, n)
  left <- integer(0)
  right <- integer(0)
  while (any(remaining)) {
    repeat {
      ids <- which(remaining)
      sinks <- ids[rowSums(B[ids, ids, drop = FALSE]) == 0]
      if (length(sinks) == 0) break
      right <- c(sinks, right)
      remaining[sinks] <- FALSE
    }
    repeat {
      ids <- which(remaining)
      sources <- ids[colSums(B[ids, ids, drop = FALSE]) == 0]
      if (length(sources) == 0) break
      left <- c(left, sources)
      remaining[sources] <- FALSE
    }
    if (any(remaining)) {
      ids <- which(remaining)
      R <- B[ids, ids, drop = FALSE]
      v <- ids[which.max(rowSums(R) - colSums(R))]
      left <- c(left, v)
      remaining[v] <- FALSE
    }
  }
  position <- integer(n)
  position[c(left, right)] <- 1:n

  # The arcs pointing backwards in the ordering break every cycle
  backward <- which(B > 0 & outer(position, position, ">"), arr.ind = TRUE)
  B[backward] <- 0

  # An arc i -> j can be restored when j does not reach i, as it then closes no
  # cycle
  removed <- 0L
  for (k in seq_len(nrow(backward))) {
    i <- backward[k, 1]
    j <- backward[k, 2]
    reached <- j
    frontier <- j
    while (length(frontier) > 0 && !(i %in% reached)) {
      children <- which(colSums(B[frontier, , drop = FALSE]) > 0)
      frontier <- setdiff(children, reached)
      reached <- c(reached, frontier)
    }
    if (i %in% reached) {
      A[i, j] <- 0
      removed <- removed + 1L
    } else {
      B[i, j] <- 1
    }
  }

  n_removed <- as.integer(n_loops) + removed
  warning(sprintf("%d arc(s) removed to enforce the DAG structure", n_removed))
  list(is_dag = FALSE, n_removed = n_removed, A = A)
}

#' @rdname dag
#' @examples
#' dag_sort(A)
#' @export

dag_sort <- function(A) {
  A <- as.matrix(A)
  if (is.null(rownames(A))) stop("A must have named rows")
  if (nrow(A) != ncol(A)) stop("A must be a square matrix")
  B <- 1 * (A > 0)
  B[is.na(B)] <- 0
  order <- .topo_order(B)
  if (is.null(order)) stop("A does not represent a DAG; run dag_check() first")
  rownames(A)[order]
}

# Kahn (1962). Returns the positions of the nodes in topological order, or NULL
# when some nodes lie on a cycle (a loop counts as a cycle)
.topo_order <- function(B) {
  indegree <- colSums(B)
  queue <- which(indegree == 0)
  order <- integer(0)
  while (length(queue) > 0) {
    v <- queue[1]
    queue <- queue[-1]
    order <- c(order, v)
    children <- which(B[v, ] > 0)
    if (length(children) > 0) {
      indegree[children] <- indegree[children] - 1
      queue <- c(queue, children[indegree[children] == 0])
    }
  }
  if (length(order) < nrow(B)) {
    return(NULL)
  }
  order
}


#' Traversal weights for main path analysis
#'
#' Computes the Search Path Count (SPC), the Search Path Link Count (SPLC), or
#' the Search Path Node Pair (SPNP) of each arc of a citation network
#' (Hummon and Doreian, 1989; Batagelj, 2003), and the weighted in-degree and
#' out-degree of each node (Kuan, 2020).
#'
#' The weight of an arc is the number of search paths that go through it. The
#' three weights differ in where the search paths start and end (Liu, Lu and
#' Ho, 2019):
#' \describe{
#'   \item{SPC}{Paths from any source to any sink. The weight of the arc
#'     \eqn{u \to v} is the number of paths from the sources to \eqn{u} times
#'     the number of paths from \eqn{v} to the sinks.}
#'   \item{SPLC}{Paths from any node to any sink, so that the intermediate papers
#'     are also origins of knowledge. The first factor becomes the number of
#'     paths from any node to \eqn{u}, counting \eqn{u} itself.}
#'   \item{SPNP}{Paths from any node to any node, so that the intermediate papers
#'     are also origins and destinations of knowledge. The second factor becomes
#'     the number of paths from \eqn{v} to any node, counting \eqn{v} itself.}
#' }
#' The three weights satisfy SPNP \eqn{\ge} SPLC \eqn{\ge} SPC. Liu, Lu and Ho
#' (2019) recommend SPLC to trace the diffusion of knowledge, and Kuan (2020)
#' argues for SPLC or SPNP.
#'
#' The weighted in-degree (WiD) and out-degree (WoD) of a node are the sums of
#' the weights of its incoming and outgoing arcs. Kuan (2020) recommends the WoD
#' of SPLC or SPNP as the weight of a paper: it counts the influence of all the
#' papers that precede it, and of the paper itself, on the papers that follow
#' it, and it is zero for the sinks, whose importance is yet to be determined.
#' The average of the two, which Kuan calls WxD, is
#' \code{(weighted_indegree + weighted_outdegree) / 2}.
#'
#' The numbers of paths grow exponentially with the length of the chains of
#' citations, so they are computed in logs to avoid overflow.
#'
#' @param A       A square, named, directed adjacency matrix in which
#'   \code{A[i,j] > 0} means that knowledge flows from paper \code{i} to paper
#'   \code{j} (paper \code{j} cites paper \code{i}). Only whether each arc is
#'   present is used. See \code{\link{dag}}.
#' @param method  One of \code{"spc"} (default), \code{"splc"}, or \code{"spnp"}.
#' @param years   Optional named numeric vector of publication years aligned with
#'   row/column names of \code{A}.  Required when \code{cutoff} is supplied.
#' @param cutoff  Optional integer year.  When provided together with
#'   \code{years}, outgoing arcs from papers published after \code{cutoff} are
#'   excluded from path counting, as a correction for right censoring: the
#'   most recent papers have not had time to be cited.  Such papers become sinks
#'   and their outgoing arcs get a weight of zero.
#' @param normalized  Whether to divide the weights by the total number of search
#'   paths of the method, so that the weight of an arc is the proportion of the
#'   search paths that go through it. The total is the number of paths from the
#'   sources to the sinks (SPC), from any node to the sinks (SPLC), or between
#'   any two nodes (SPNP), counting only paths with at least one arc.
#'
#' @return A named list:
#' \describe{
#'   \item{\code{edge_weights}}{Square matrix of the same dimensions as
#'     \code{A} containing the chosen traversal weight for each arc.}
#'   \item{\code{weighted_indegree}}{Named numeric vector with the sum of the
#'     weights of the incoming arcs of each node (WiD).}
#'   \item{\code{weighted_outdegree}}{Named numeric vector with the sum of the
#'     weights of the outgoing arcs of each node (WoD).}
#'   \item{\code{log_forward_source}}{Log of the number of paths from the sources
#'     to each node.}
#'   \item{\code{log_forward_all}}{Log of the number of paths from any node to
#'     each node, counting the node itself.}
#'   \item{\code{log_backward_sink}}{Log of the number of paths from each node to
#'     the sinks.}
#'   \item{\code{log_backward_all}}{Log of the number of paths from each node to
#'     any node, counting the node itself.}
#'   \item{\code{sources}}{Character vector of source node names.}
#'   \item{\code{sinks}}{Character vector of sink node names.}
#'   \item{\code{total_log_paths}}{Log of the total number of search paths of
#'     the method (see \code{normalized}).}
#'   \item{\code{method}}{The method string used.}
#' }
#'
#' @references
#'
#' Batagelj, V. (2003). Efficient algorithms for citation network analysis.
#' arXiv:cs/0309023.
#'
#' Hummon, N.P. and Doreian, P. (1989). Connectivity in a citation network:
#' The development of DNA theory. Social Networks. 11(1): 39-63.
#' \doi{10.1016/0378-8733(89)90017-8}.
#'
#' Kuan, C. H. (2020). Regarding weight assignment algorithms of main path
#' analysis and the conversion of arc weights to node weights. Scientometrics,
#' 124(1), 775-782. \doi{10.1007/s11192-020-03468-8}.
#'
#' Liu, J.S. and Lu, L.Y.Y. (2012). An integrated approach for main path
#' analysis: Development of the Hirsch index as an example. Journal of the
#' American Society for Information Science and Technology. 63(3): 528-542.
#' \doi{10.1002/asi.21692}.
#'
#' Liu, J.S., Lu, L.Y.Y. and Ho, M.H.C. (2019). A few notes on main path
#' analysis. Scientometrics. 119(1): 379-391. \doi{10.1007/s11192-019-03034-x}.
#'
#' Verspagen, B. (2007). Mapping technological trajectories as patent citation
#' networks: A study on the history of fuel cell research. Advances in Complex
#' Systems. 10(1): 93-115. \doi{10.1142/S0219525907000945}.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' # Network of Fig. 2 in Kuan (2020)
#' A <- matrix(c(
#'  0,0,0,1,0,0,0,0,0,0,0,0,0,
#'  0,0,0,1,0,0,0,0,0,0,0,0,0,
#'  0,0,0,1,0,0,0,0,0,0,0,0,0,
#'  0,0,0,0,1,1,0,0,0,0,0,0,0,
#'  0,0,0,0,0,0,1,0,0,0,0,0,0,
#'  0,0,0,0,0,0,0,1,0,0,0,0,0,
#'  0,0,0,0,0,0,0,1,1,1,1,0,0,
#'  0,0,0,0,0,0,0,0,0,0,0,1,1,
#'  0,0,0,0,0,0,0,0,0,0,0,0,0,
#'  0,0,0,0,0,0,0,0,0,0,0,0,0,
#'  0,0,0,0,0,0,0,0,0,0,0,0,0,
#'  0,0,0,0,0,0,0,0,0,0,0,0,0,
#'  0,0,0,0,0,0,0,0,0,0,0,0,0
#'), nrow = 13, byrow = TRUE)
#'
#' rownames(A) <- colnames(A) <- 1:13
#'
#' spc <- traversal_weights(A, method = "spc")
#' splc <- traversal_weights(A, method = "splc")
#' spnp <- traversal_weights(A, method = "spnp")
#'
#' # Arc 8 -> 12 has SPC 6 and SPLC 12; arc 1 -> 4 has SPNP 13 (Kuan, 2020)
#' spc$edge_weights["8", "12"]
#' splc$edge_weights["8", "12"]
#' spnp$edge_weights["1", "4"]
#'
#' # Table 4 in Kuan (2020)
#' cbind(
#'   SPLC_WoD = splc$weighted_outdegree,
#'   SPNP_WoD = spnp$weighted_outdegree,
#'   SPNP_WxD = (spnp$weighted_indegree + spnp$weighted_outdegree) / 2
#' )
#' @export

traversal_weights <- function(A,
                              method = c("spc", "splc", "spnp"),
                              years = NULL,
                              cutoff = NULL,
                              normalized = FALSE) {
  method <- match.arg(method)
  A <- as.matrix(A)
  if (is.null(rownames(A)) || is.null(colnames(A))) stop("A must have named rows and columns")
  if (nrow(A) != ncol(A)) stop("A must be a square matrix")
  if (!is.null(cutoff) && is.null(years)) stop("years must be provided together with cutoff")

  nms <- rownames(A)
  n <- nrow(A)
  B <- 1 * (A > 0)
  B[is.na(B)] <- 0

  if (!is.null(cutoff)) {
    years <- years[nms]
    if (anyNA(years)) warning("Some nodes have no publication year; their outgoing arcs are included in path counting")
    post <- !is.na(years) & years > cutoff
    B[post, ] <- 0
  }
  if (sum(B) == 0) stop("A has no arcs")

  topo_ord <- dag_sort(B)

  sources <- nms[colSums(B) == 0]
  sinks <- nms[rowSums(B) == 0]

  # Paths that reach each node: from the sources (SPC), or from any node,
  # counting the node itself as a path of length zero (SPLC and SPNP)
  log_forward_source <- setNames(rep(-Inf, n), nms)
  log_forward_source[sources] <- 0
  log_forward_all <- setNames(rep(0, n), nms)
  for (v in topo_ord) {
    parents <- nms[B[, v] > 0]
    if (length(parents) > 0) {
      log_forward_source[v] <- .log_sum_exp(log_forward_source[parents])
      log_forward_all[v] <- .log_sum_exp(c(0, log_forward_all[parents]))
    }
  }

  # Paths that leave each node: towards the sinks (SPC and SPLC), or towards any
  # node, counting the node itself (SPNP)
  log_backward_sink <- setNames(rep(-Inf, n), nms)
  log_backward_sink[sinks] <- 0
  log_backward_all <- setNames(rep(0, n), nms)
  for (v in rev(topo_ord)) {
    children <- nms[B[v, ] > 0]
    if (length(children) > 0) {
      log_backward_sink[v] <- .log_sum_exp(log_backward_sink[children])
      log_backward_all[v] <- .log_sum_exp(c(0, log_backward_all[children]))
    }
  }

  # The arc u -> v is on every search path that reaches u and continues from v.
  # The total counts the search paths with at least one arc: they end at a sink
  # that is not isolated (SPC and SPLC) or at any node (SPNP), and exp(x) - 1
  # removes the path of length zero from each end
  reached <- colSums(B) > 0
  if (method == "spc") {
    log_tail <- log_forward_source
    log_head <- log_backward_sink
    total_log_paths <- .log_sum_exp(log_forward_source[sinks[reached[sinks]]])
  }
  if (method == "splc") {
    log_tail <- log_forward_all
    log_head <- log_backward_sink
    ends <- log_forward_all[sinks]
    total_log_paths <- .log_sum_exp(ends + log1p(-exp(-ends)))
  }
  if (method == "spnp") {
    log_tail <- log_forward_all
    log_head <- log_backward_all
    total_log_paths <- .log_sum_exp(log_forward_all + log1p(-exp(-log_forward_all)))
  }

  LOG_W <- outer(log_tail, log_head, "+")
  if (normalized) {
    LOG_W <- LOG_W - total_log_paths
  }
  edge_weights <- matrix(0, n, n, dimnames = list(nms, nms))
  edge_weights[B > 0] <- exp(LOG_W[B > 0])

  list(
    edge_weights = edge_weights,
    weighted_indegree = colSums(edge_weights),
    weighted_outdegree = rowSums(edge_weights),
    log_forward_source = log_forward_source,
    log_forward_all = log_forward_all,
    log_backward_sink = log_backward_sink,
    log_backward_all = log_backward_all,
    sources = sources,
    sinks = sinks,
    total_log_paths = total_log_paths,
    method = method
  )
}


#' Temporal decay weighting for citation edges
#'
#' Applies an exponential temporal decay to the arcs of a citation adjacency
#' matrix and optionally normalises the result so that each citing paper
#' distributes exactly one unit of citation influence among its references.
#'
#' As in \code{\link{traversal_weights}}, \code{A[i,j] > 0} means that paper
#' \code{j} cites paper \code{i}. The weight of the arc \eqn{i \to j} is
#' \eqn{\exp(-\lambda \cdot (y_j - y_i))} where \eqn{y_i} and \eqn{y_j} are
#' the publication years of \eqn{i} and \eqn{j} respectively.  A larger
#' \eqn{\lambda} discounts older citations more aggressively.  The
#' normalisation divides each column, which holds the references of a citing
#' paper, by its sum, so that differences in the length of the reference lists
#' do not inflate the raw weights.
#'
#' @param A         A square, named, directed adjacency matrix.
#' @param years     Named numeric vector of publication years aligned with
#'   row/column names of \code{A}.
#' @param lambda    Positive numeric decay rate.  Default \code{0.2}.
#' @param normalize Logical; if \code{TRUE} (default) each column is divided by
#'   its sum so that the references of each citing paper sum to one.
#'
#' @return A numeric matrix of the same dimensions as \code{A} containing the
#'   decay-weighted (and optionally normalised) citation arc weights.
#'   \code{\link{traversal_weights}} uses only whether each arc is present, so
#'   these weights do not change the search path counts; they can be combined
#'   with them, for instance by multiplying the two matrices element by element.
#'
#' @references
#'
#' Hummon, N.P. and Doreian, P. (1989). Connectivity in a citation network:
#' The development of DNA theory. Social Networks. 11(1): 39-63.
#' \doi{10.1016/0378-8733(89)90017-8}.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' # P1 is cited by P2 and P3, which are both cited by P4
#' A <- matrix(c(
#'   0, 1, 1, 0,
#'   0, 0, 0, 1,
#'   0, 0, 0, 1,
#'   0, 0, 0, 0
#' ), byrow = TRUE, nrow = 4)
#' rownames(A) <- c("P1", "P2", "P3", "P4")
#' colnames(A) <- c("P1", "P2", "P3", "P4")
#' years <- c(P1 = 2000, P2 = 2005, P3 = 2006, P4 = 2010)
#'
#' citation_decay(A, years, lambda = 0.2)
#' @export

citation_decay <- function(A, years, lambda = 0.2, normalize = TRUE) {
  A <- as.matrix(A)
  if (is.null(rownames(A)) || is.null(colnames(A))) stop("A must have named rows and columns")
  if (nrow(A) != ncol(A)) stop("A must be a square matrix")
  if (!is.numeric(lambda) || lambda <= 0) stop("lambda must be a positive number")

  nms <- rownames(A)
  years <- years[nms]

  # Years between the cited paper (row) and the citing paper (column)
  dt_mat <- outer(years, years, function(cited, citing) citing - cited)
  if (any(A > 0 & dt_mat <= 0, na.rm = TRUE)) {
    warning("Some arcs have non-positive temporal distance (citing year <= cited year); check DAG assumption")
  }

  decay_mat <- exp(-lambda * pmax(dt_mat, 0))
  W <- A * decay_mat
  W[is.na(W)] <- 0

  if (normalize) {
    cs <- colSums(W)
    cs[cs == 0] <- 1
    W <- t(t(W) / cs)
  }
  W
}


#' Main path extraction from a citation network
#'
#' Extracts the main path or key-route network from a directed citation
#' adjacency matrix using traversal weights (SPC, SPLC, or SPNP).
#'
#' Three extraction strategies are available:
#' \describe{
#'   \item{\code{"global"}}{Finds the single source-to-sink path that
#'     maximises the total accumulated edge weight, using dynamic programming
#'     along the topological order.}
#'   \item{\code{"local"}}{Traces backward and forward from each node in
#'     \code{seeds}, always following the edge with the highest weight.
#'     Returns one route per seed.}
#'   \item{\code{"key_route"}}{Ranks all edges by weight descending; for each
#'     of the top \code{k} seed edges not yet covered by a previous route,
#'     traces backward from the edge's tail and forward from the edge's head.
#'     The union of all routes forms a sub-DAG capturing multiple intellectual
#'     trajectories (Liu & Lu, 2012).}
#' }
#'
#' @param A           A square, named, directed adjacency matrix in which
#'   \code{A[i,j] > 0} means that paper \code{j} cites paper \code{i}, so that
#'   the paths follow the flow of knowledge. See \code{\link{dag}}.
#' @param weights     Output of \code{traversal_weights()}.  Computed
#'   internally with \code{weight_type} when \code{NULL}.
#' @param method      One of \code{"global"} (default), \code{"local"}, or
#'   \code{"key_route"}.
#' @param weight_type One of \code{"spc"} (default), \code{"splc"}, or
#'   \code{"spnp"}.  Ignored when \code{weights} is supplied.
#' @param k           Integer number of seed routes for \code{method =
#'   "key_route"}.  Default \code{1L}.
#' @param seeds       Character vector of seed node names for \code{method =
#'   "local"}.
#'
#' @return A named list:
#' \describe{
#'   \item{\code{nodes}}{Character vector of node names on the main path or
#'     key-route network (union across all routes).}
#'   \item{\code{edges}}{Square adjacency matrix restricted to path nodes and
#'     edges, with the same values as \code{A} for included edges and zero
#'     elsewhere.}
#'   \item{\code{routes}}{List of character vectors, one per extracted route,
#'     each giving the ordered sequence of node names.}
#'   \item{\code{weights}}{The \code{traversal_weights()} result used.}
#' }
#'
#' @references
#'
#' Hummon, N.P. and Doreian, P. (1989). Connectivity in a citation network:
#' The development of DNA theory. Social Networks. 11(1): 39-63.
#' \doi{10.1016/0378-8733(89)90017-8}.
#'
#' Liu, J.S. and Lu, L.Y.Y. (2012). An integrated approach for main path
#' analysis: Development of the Hirsch index as an example. Journal of the
#' American Society for Information Science and Technology. 63(3): 528-542.
#' \doi{10.1002/asi.21692}.
#'
#' Lucio-Arias, D. and Leydesdorff, L. (2008). Main-path analysis and
#' path-dependent transitions in HistCite-based historiographs. Journal of the
#' American Society for Information Science and Technology. 59(12): 1948-1962.
#' \doi{10.1002/asi.20903}.
#'
#' Verspagen, B. (2007). Mapping technological trajectories as patent citation
#' networks. Advances in Complex Systems. 10(1): 93-115.
#' \doi{10.1142/S0219525907000945}.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   0, 0, 0, 1, 1, 0,
#'   0, 0, 0, 0, 1, 0,
#'   0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 1,
#'   0, 0, 0, 0, 0, 0
#' ), byrow = TRUE, nrow = 6)
#' rownames(A) <- letters[1:6]
#' colnames(A) <- letters[1:6]
#'
#' mp <- main_path(A, method = "global")
#' mp$routes
#'
#' kr <- main_path(A, method = "key_route", k = 2L)
#' kr$nodes
#' @export

main_path <- function(A,
                      weights     = NULL,
                      method      = c("global", "local", "key_route"),
                      weight_type = c("spc", "splc", "spnp"),
                      k           = 1L,
                      seeds       = NULL) {
  method      <- match.arg(method)
  weight_type <- match.arg(weight_type)
  A <- as.matrix(A)
  if (is.null(rownames(A)) || is.null(colnames(A)))
    stop("A must have named rows and columns")

  nms <- rownames(A)

  if (is.null(weights))
    weights <- traversal_weights(A, method = weight_type)

  edge_w <- weights$edge_weights

  if (method == "global") {
    topo_ord <- dag_sort(A)
    n        <- length(nms)
    dp       <- setNames(rep(-Inf, n), nms)
    pred     <- setNames(rep(NA_character_, n), nms)
    dp[weights$sources] <- 0

    for (v in topo_ord) {
      in_u <- nms[edge_w[, v] > 0]
      if (length(in_u) == 0L) next
      cands <- dp[in_u] + edge_w[in_u, v]
      best  <- names(which.max(cands))
      if (is.finite(cands[best]) && cands[best] > dp[v]) {
        dp[v]   <- cands[best]
        pred[v] <- best
      }
    }

    reach_sinks <- weights$sinks[is.finite(dp[weights$sinks])]
    if (length(reach_sinks) == 0L)
      stop("No source-to-sink paths found")
    end     <- names(which.max(dp[reach_sinks]))
    path    <- end
    current <- end
    while (!is.na(pred[current])) {
      current <- unname(pred[current])
      path    <- c(current, path)
    }
    routes <- list(unname(path))
  }

  if (method == "local") {
    if (is.null(seeds))
      stop("seeds must be provided for method = 'local'")
    seeds <- intersect(seeds, nms)
    if (length(seeds) == 0L)
      stop("None of the seeds match node names in A")
    routes <- lapply(seeds, function(s) {
      unique(c(.trace_back(s, edge_w), .trace_fwd(s, edge_w)))
    })
  }

  if (method == "key_route") {
    idx_mat  <- which(edge_w > 0, arr.ind = TRUE)
    if (nrow(idx_mat) == 0L)
      stop("Edge weight matrix has no nonzero entries")
    from_vec <- nms[idx_mat[, 1L]]
    to_vec   <- nms[idx_mat[, 2L]]
    w_vec    <- edge_w[idx_mat]
    order_d  <- order(-w_vec)

    covered <- logical(length(w_vec))
    routes  <- list()
    n_found <- 0L

    for (i in order_d) {
      if (n_found >= k) break
      if (covered[i]) next

      u <- from_vec[i]
      v <- to_vec[i]

      back        <- .trace_back(u, edge_w)
      fwd         <- .trace_fwd(v, edge_w)
      route_nodes <- unique(c(back, fwd))

      if (length(route_nodes) >= 2L) {
        route_pairs <- paste(route_nodes[-length(route_nodes)],
                             route_nodes[-1L])
        covered[paste(from_vec, to_vec) %in% route_pairs] <- TRUE
      }

      n_found          <- n_found + 1L
      routes[[n_found]] <- route_nodes
    }

    if (length(routes) == 0L)
      warning("No routes extracted; the edge weight matrix may have no eligible edges")
  }

  all_nodes <- unique(unlist(routes))

  route_pairs <- unique(unlist(lapply(routes, function(r) {
    if (length(r) < 2L) return(character(0))
    paste(r[-length(r)], r[-1L])
  })))

  idx_all    <- which(A > 0, arr.ind = TRUE)
  from_all   <- rownames(A)[idx_all[, 1L]]
  to_all     <- colnames(A)[idx_all[, 2L]]
  keep       <- paste(from_all, to_all) %in% route_pairs

  path_A <- matrix(0, length(all_nodes), length(all_nodes),
                   dimnames = list(all_nodes, all_nodes))
  for (j in which(keep)) {
    fu <- from_all[j]; tu <- to_all[j]
    if (fu %in% all_nodes && tu %in% all_nodes)
      path_A[fu, tu] <- A[fu, tu]
  }

  list(nodes = all_nodes, edges = path_A, routes = routes, weights = weights)
}


#' Diagnostics for main path analysis
#'
#' Computes K-sensitivity of the key-route network and the Jaccard overlap
#' between SPC- and SPLC-based key routes as a robustness check.
#'
#' The K-sensitivity table shows how the size of the key-route network grows
#' as more seed routes are added.  Stabilisation of new-node counts signals
#' that the main structural backbone has been captured.
#'
#' The SPLC/SPC Jaccard overlap at \code{k_jaccard} routes measures whether
#' the key routes change when the intermediate papers are also counted as
#' origins of knowledge (SPLC) instead of only the sources (SPC), which is the
#' main difference between the two weights (Liu, Lu and Ho, 2019).  A value of
#' one means that both weights give the same papers.
#'
#' @param A          A square, named, directed adjacency matrix in which
#'   \code{A[i,j] > 0} means that paper \code{j} cites paper \code{i}.
#' @param weights    Output of \code{traversal_weights()} with
#'   \code{method = "spc"}.  Computed internally when \code{NULL}.
#' @param k_values   Integer vector of K values for the sensitivity table.
#'   Default \code{c(5, 10, 15, 20, 30)}.
#' @param k_jaccard  Integer K used for the SPLC/SPC Jaccard comparison.
#'   Default \code{10L}.
#'
#' @return A named list:
#' \describe{
#'   \item{\code{k_sensitivity}}{Data frame with columns \code{K},
#'     \code{n_nodes}, \code{n_edges}, and \code{new_nodes} (marginal nodes
#'     added at each K).}
#'   \item{\code{splc_spc_jaccard}}{Numeric Jaccard overlap of node sets
#'     between SPLC- and SPC-based key routes at \code{k_jaccard}.}
#'   \item{\code{weight_summary}}{Summary statistics for nonzero edge weights.}
#'   \item{\code{n_sources}}{Number of source nodes.}
#'   \item{\code{n_sinks}}{Number of sink nodes.}
#'   \item{\code{total_log_paths}}{Log of the total number of search paths of
#'     \code{weights}.}
#' }
#'
#' @references
#'
#' Liu, J.S., Lu, L.Y.Y. and Ho, M.H.C. (2019). A few notes on main path
#' analysis. Scientometrics. 119(1): 379-391. \doi{10.1007/s11192-019-03034-x}.
#'
#' Verspagen, B. (2007). Mapping technological trajectories as patent citation
#' networks. Advances in Complex Systems. 10(1): 93-115.
#' \doi{10.1142/S0219525907000945}.
#'
#' @author Alejandro Espinosa-Rada
#'
#' @examples
#' A <- matrix(c(
#'   0, 1, 1, 0, 0, 0,
#'   0, 0, 0, 1, 1, 0,
#'   0, 0, 0, 0, 1, 0,
#'   0, 0, 0, 0, 0, 0,
#'   0, 0, 0, 0, 0, 1,
#'   0, 0, 0, 0, 0, 0
#' ), byrow = TRUE, nrow = 6)
#' rownames(A) <- letters[1:6]
#' colnames(A) <- letters[1:6]
#'
#' main_path_diag(A, k_values = c(1L, 2L, 3L))
#' @export

main_path_diag <- function(A,
                            weights   = NULL,
                            k_values  = c(5L, 10L, 15L, 20L, 30L),
                            k_jaccard = 10L) {
  A <- as.matrix(A)
  if (is.null(rownames(A)) || is.null(colnames(A)))
    stop("A must have named rows and columns")

  if (is.null(weights))
    weights <- traversal_weights(A, method = "spc")

  k_sens <- lapply(k_values, function(kk) {
    kr <- main_path(A, weights = weights, method = "key_route", k = kk)
    data.frame(K = kk, n_nodes = length(kr$nodes),
               n_edges = sum(kr$edges > 0), stringsAsFactors = FALSE)
  })
  k_sens_df          <- do.call(rbind, k_sens)
  k_sens_df$new_nodes <- c(NA_integer_, diff(k_sens_df$n_nodes))

  splc_w  <- traversal_weights(A, method = "splc")
  kr_spc  <- main_path(A, weights = weights, method = "key_route", k = k_jaccard)
  kr_splc <- main_path(A, weights = splc_w,  method = "key_route", k = k_jaccard)
  jaccard <- length(intersect(kr_spc$nodes, kr_splc$nodes)) /
             length(union(kr_spc$nodes,     kr_splc$nodes))

  ew_nz <- weights$edge_weights[weights$edge_weights > 0]

  list(
    k_sensitivity    = k_sens_df,
    splc_spc_jaccard = jaccard,
    weight_summary   = summary(ew_nz),
    n_sources        = length(weights$sources),
    n_sinks          = length(weights$sinks),
    total_log_paths  = weights$total_log_paths
  )
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

.log_sum_exp <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(-Inf)
  m <- max(x)
  m + log(sum(exp(x - m)))
}

.trace_back <- function(start, edge_weights) {
  path    <- start
  current <- start
  repeat {
    col_w    <- edge_weights[, current]
    eligible <- col_w[col_w > 0 & !(names(col_w) %in% path)]
    if (length(eligible) == 0L) break
    best    <- names(which.max(eligible))
    path    <- c(best, path)
    current <- best
  }
  path
}

.trace_fwd <- function(start, edge_weights) {
  path    <- start
  current <- start
  repeat {
    row_w    <- edge_weights[current, ]
    eligible <- row_w[row_w > 0 & !(names(row_w) %in% path)]
    if (length(eligible) == 0L) break
    best    <- names(which.max(eligible))
    path    <- c(path, best)
    current <- best
  }
  path
}
