#' @title GMDH COMBI-TWICE auxiliar functions
#'
#' @description Performs auxiliar tasks to predict.mia
#'
#' @keywords internal
#'
gmdh.combi.twice_3 <- function(X, y, G) {

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

    regressors <- fun.poly(X, G = G)

    combs <- do.call(expand.grid, rep(list(c(FALSE, TRUE)), ncol(regressors)))[-1, ]

    results <- apply(combs, 1, function(x){fun.svd_3(y = y, x = regressors[, x, drop = FALSE])})
    names(results) <- paste(lap, c("."), 1:length(results), sep = "")

    cv <- unlist(lapply(results, function(x){x$CV}))
    combos <- !duplicated(cv, MARGIN = 2)
    cv <- cv[combos, drop = FALSE]
    ndx <- sort(na.omit(order(cv)[1:prune]))
    cv <- cv[ndx, drop = FALSE]

    fin.1 <- min(cv, na.rm = TRUE)
    results <- results[names(cv)]

    Z <- lapply(results, predict.svd, regressors)
    Z <- matrix(data = unlist(Z), nrow = nrow(X), ncol = length(Z))
    colnames(Z) <- names(results)

    lap <- lap + 1
    message(paste("Layer ", lap, sep = ""))

    regressors <- fun.poly(Z, G = G)

    combs <- do.call(expand.grid, rep(list(c(FALSE, TRUE)), ncol(regressors)))[-1, ]

    results.2 <- apply(combs, 1, function(x){fun.svd_3(y = y, x = regressors[, x, drop = FALSE])})
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

    X <- lapply(results.2, predict.svd, regressors)
    X <- matrix(data = unlist(X), nrow = nrow(Z), ncol = length(X))
    colnames(X) <- names(results.2)
  }
}
