#' @title GMDH COMBI-TWICE auxiliar functions
#'
#' @description Performs auxiliar tasks to predict.mia
#'
#' @keywords internal
#'
gmdh.combi.twice_2 <- function(X, y, G, x.test, y.test) {

  fin.1 <- Inf
  fin.2 <- 0
  lap <- 0

  modelos <- vector(mode = "list", length = 2)
  names(modelos) <- c("results", "G")
  modelos$G <- G
  prune <- ncol(X)

  while(fin.1 >= fin.2) {

    lap <- lap + 1
    message(paste("Layer ", lap, sep = ""))

    X <- fun.poly(X, G = G)
    x.test <- fun.poly(x.test, G = G)

    combs <- do.call(expand.grid, rep(list(c(FALSE, TRUE)), ncol(X)))[-1, ]

    results <- apply(combs, 1, function(x){fun.svd_2(y = y,
                                                     x = X[, x, drop = FALSE],
                                                     x.test = x.test[, x, drop = FALSE],
                                                     y.test = y.test)})

    names(results) <- paste(lap, c("."), 1:length(results), sep = "")

    cv <- unlist(lapply(results, function(x){x$CV}))
    combos <- !duplicated(cv, MARGIN = 2)
    cv <- cv[combos, drop = FALSE]
    ndx <- sort(na.omit(order(cv)[1:prune]))
    cv <- cv[ndx, drop = FALSE]

    fin.1 <- min(cv, na.rm = TRUE)
    results <- results[names(cv)]

    Z <- lapply(results, predict.svd, X)
    Z <- matrix(data = unlist(Z), nrow = nrow(X), ncol = length(Z))
    Z.test <- lapply(results, predict.svd, x.test)
    Z.test <- matrix(data = unlist(Z.test), nrow = nrow(x.test), ncol = length(Z.test))

    colnames(Z) <- colnames(Z.test) <- names(results)

    lap <- lap + 1
    message(paste("Layer ", lap, sep = ""))

    Z <- fun.poly(Z, G = G)
    Z.test <- fun.poly(Z.test, G = G)

    combs <- do.call(expand.grid, rep(list(c(FALSE, TRUE)), ncol(Z)))[-1, ]

    results.2 <- apply(combs, 1, function(x){fun.svd_2(y = y,
                                                       x = Z[, x, drop = FALSE],
                                                       x.test = Z.test[, x, drop = FALSE],
                                                       y.test = y.test)})

    names(results.2) <- paste(lap, c("."), 1:length(results.2), sep = "")

    cv <- unlist(lapply(results.2, function(x){x$CV}))
    combos <- !duplicated(cv, MARGIN = 2)
    cv <- cv[combos, drop = FALSE]
    ndx <- sort(na.omit(order(cv)[1:prune]))
    cv <- cv[ndx, drop = FALSE]

    fin.2 <- min(cv, na.rm = TRUE)
    results.2 <- results.2[ndx]

    modelos$results[[length(modelos$results) + 1]] <- results
    modelos$results[[length(modelos$results) + 1]] <- results.2
    class(modelos) <- "combitwice"

    ifelse(fin.2 >= fin.1, return(modelos), NA)

    X <- lapply(results.2, predict.svd, Z)
    X <- matrix(data = unlist(X), nrow = nrow(Z), ncol = length(X))
    x.test <- lapply(results.2, predict.svd, Z.test)
    x.test <- matrix(data = unlist(x.test), nrow = nrow(Z.test), ncol = length(x.test))
    colnames(X) <- colnames(x.test) <- names(results.2)
  }
}
