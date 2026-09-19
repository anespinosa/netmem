#' Social influence
#'
#' Simulates how the opinions of the actors change when they are influenced by their
#' neighbours, under the rules compared by Flache et al. (2017).
#'
#' Every actor starts with an opinion and updates it at each step:
#'
#' \code{assimilation}: the actor moves towards the opinions of its neighbours, which leads the
#' network to consensus (French, 1956; DeGroot, 1974).
#'
#' \code{bounded}: the actor is only influenced by the neighbours whose opinion differs less
#' than \code{epsilon}, which leads to fragmentation into groups that no longer influence each
#' other (Hegselmann and Krause, 2002).
#'
#' \code{repulsion}: the actor moves towards similar neighbours and away from those who are too
#' different, which leads to bi-polarization. The opinions are kept between zero and one.
#'
#' \code{friedkin}: the actor combines the opinions of its neighbours with the opinion it
#' started with, and \code{susceptibility} is the weight given to the neighbours
#' (Friedkin and Johnsen, 1990).
#'
#' @param A   A square matrix of influence, which can be weighted. The rows are usually normalised so that the weights of each actor add up to one
#' @param opinion   A vector with the initial opinion of every actor, usually between zero and one
#' @param rule   The rule of influence: \code{assimilation} (default), \code{bounded}, \code{repulsion} or \code{friedkin}
#' @param mu   How much an actor moves at each step
#' @param epsilon   Maximum difference of opinion that still influences an actor, for the \code{bounded} rule
#' @param susceptibility   Weight given to the neighbours in the \code{friedkin} rule, as a number or a vector with one value per actor
#' @param steps   Number of steps
#'
#' @return This function returns the opinions of every actor at every step, the final opinions, and the number of groups of opinions at the end.
#'
#' @references
#'
#' DeGroot, M. H. (1974). Reaching a consensus. Journal of the American Statistical Association, 69(345), 118–121. \doi{10.1080/01621459.1974.10480137}
#'
#' Flache, A., Mas, M., Feliciani, T., Chattoe-Brown, E., Deffuant, G., Huet, S. and Lorenz, J. (2017). Models of social influence: Towards the next frontiers. Journal of Artificial Societies and Social Simulation, 20(4), 2. \doi{10.18564/jasss.3521}
#'
#' French, J. R. P. (1956). A formal theory of social power. Psychological Review, 63(3), 181–194. \doi{10.1037/h0046123}
#'
#' Friedkin, N. E. and Johnsen, E. C. (1990). Social influence and opinions. Journal of Mathematical Sociology, 15(3-4), 193–206. \doi{10.1080/0022250X.1990.9990069}
#'
#' Hegselmann, R. and Krause, U. (2002). Opinion dynamics and bounded confidence models, analysis and simulation. Journal of Artificial Societies and Social Simulation, 5(3), 2.
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
#' W <- A / rowSums(A)
#' opinion <- c(0.1, 0.2, 0.3, 0.7, 0.8, 0.9)
#'
#' social_influence(W, opinion, rule = "assimilation", steps = 20)$final
#' social_influence(W, opinion, rule = "bounded", epsilon = 0.15, steps = 20)$groups
#' @export

social_influence <- function(A, opinion,
                             rule = c("assimilation", "bounded", "repulsion", "friedkin"),
                             mu = 0.3, epsilon = 0.15, susceptibility = 0.5, steps = 50) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (length(opinion) != nrow(A)) stop("There should be one opinion for each actor")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  rule <- match.arg(rule)
  n <- nrow(A)

  trajectory <- matrix(NA, steps + 1, n)
  colnames(trajectory) <- rownames(A)
  trajectory[1, ] <- opinion
  initial <- opinion

  for (t in seq_len(steps)) {
    if (rule == "assimilation") {
      opinion <- opinion + mu * as.numeric(A %*% opinion - rowSums(A) * opinion)
    }

    if (rule == "friedkin") {
      opinion <- susceptibility * as.numeric(A %*% opinion) + (1 - susceptibility) * initial
    }

    if (rule == "bounded") {
      updated <- opinion
      for (i in 1:n) {
        neighbours <- which(A[i, ] > 0)
        if (length(neighbours) == 0) next
        # Only the neighbours with a similar enough opinion are influential
        close <- neighbours[abs(opinion[neighbours] - opinion[i]) <= epsilon]
        if (length(close) == 0) next
        weights <- A[i, close] / sum(A[i, close])
        updated[i] <- opinion[i] + mu * sum(weights * (opinion[close] - opinion[i]))
      }
      opinion <- updated
    }

    if (rule == "repulsion") {
      updated <- opinion
      for (i in 1:n) {
        neighbours <- which(A[i, ] > 0)
        if (length(neighbours) == 0) next
        # The influence is negative when the opinions differ by more than a half
        weight <- mu * (1 - 2 * abs(opinion[neighbours] - opinion[i]))
        updated[i] <- opinion[i] + sum(A[i, neighbours] * weight * (opinion[neighbours] - opinion[i]))
      }
      opinion <- pmin(pmax(updated, 0), 1)
    }

    trajectory[t + 1, ] <- opinion
  }

  return(list(
    trajectory = trajectory,
    final = opinion,
    groups = opinion_groups(opinion)
  ))
}

# Number of groups of opinions, i.e. values that are not separated by more than the tolerance
opinion_groups <- function(opinion, tol = 0.05) {
  sorted <- sort(opinion)
  groups <- 1
  for (k in seq_along(sorted)[-1]) {
    if (sorted[k] - sorted[k - 1] > tol) groups <- groups + 1
  }
  groups
}


#' Threshold diffusion
#'
#' Simulates the diffusion of a behaviour through the network, where an actor adopts it when
#' enough of its neighbours have already adopted it (Granovetter, 1978; Valente, 1996).
#'
#' At each step, the actors that have not adopted count how many of their neighbours did. They
#' adopt when that number, or that proportion of their neighbours, reaches their threshold. The
#' actors that adopt never go back, so the process stops when nobody else adopts.
#'
#' @param A   A square matrix
#' @param seeds   The actors that have adopted at the beginning, by name or by position
#' @param threshold   The threshold of every actor, as a single value or a vector
#' @param mode   Whether the threshold is a \code{proportion} of the neighbours (default) or a \code{count} of them
#' @param steps   Maximum number of steps. If NULL, the process runs until nobody else adopts
#'
#' @return This function returns who has adopted at every step, the step in which every actor adopted, and the proportion of actors that adopted.
#'
#' @references
#'
#' Granovetter, M. (1978). Threshold models of collective behavior. American Journal of Sociology, 83(6), 1420–1443. \doi{10.1086/226707}
#'
#' Valente, T. W. (1996). Social network thresholds in the diffusion of innovations. Social Networks, 18(1), 69–89. \doi{10.1016/0378-8733(95)00256-1}
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
#' threshold_diffusion(A, seeds = c("a", "b"), threshold = 0.5)
#' @export

threshold_diffusion <- function(A, seeds, threshold = 0.5,
                                mode = c("proportion", "count"), steps = NULL) {
  A <- as.matrix(A)
  if (nrow(A) != ncol(A)) stop("Matrix should be square")
  if (any(is.na(A) == TRUE)) {
    A <- ifelse(is.na(A), 0, A)
  }
  mode <- match.arg(mode)
  n <- nrow(A)
  if (is.null(rownames(A))) {
    rownames(A) <- as.character(seq_len(n))
    colnames(A) <- rownames(A)
  }
  A[A > 0] <- 1
  diag(A) <- 0
  if (is.null(steps)) {
    steps <- n
  }
  if (length(threshold) == 1) {
    threshold <- rep(threshold, n)
  }
  if (length(threshold) != n) stop("There should be one threshold for each actor")

  adopted <- rep(FALSE, n)
  names(adopted) <- rownames(A)
  if (is.character(seeds)) {
    seeds <- match(seeds, rownames(A))
  }
  if (any(is.na(seeds))) stop("The seeds do not match the names of the nodes")
  adopted[seeds] <- TRUE

  time <- rep(NA, n)
  names(time) <- rownames(A)
  time[adopted] <- 0
  history <- matrix(adopted, nrow = 1, dimnames = list(NULL, rownames(A)))

  for (t in seq_len(steps)) {
    # An actor is exposed to the neighbours that have already adopted
    exposure <- as.numeric(A %*% adopted)
    if (mode == "proportion") {
      neighbours <- rowSums(A)
      exposure <- ifelse(neighbours > 0, exposure / neighbours, 0)
    }
    new <- !adopted & exposure >= threshold
    if (!any(new)) break
    adopted[new] <- TRUE
    time[new] <- t
    history <- rbind(history, adopted)
  }
  rownames(history) <- paste0("t", seq_len(nrow(history)) - 1)

  return(list(
    history = history,
    time = time,
    adopters = sum(adopted) / n
  ))
}
