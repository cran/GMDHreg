#' @title GMDH COMBI auxiliar functions
#'
#' @description Build a regression model performing GMDH Combinatorial with PRESS criteria.
#'
#' @keywords internal
#'
gmdh.combi_1 <- function(X, y, G = 2) {

  results <- vector(mode = "list", length = 2)
  names(results) <- c("results", "G")
  regressors <- fun.poly(X, G = G)
  regressors <- fun.filter(regressors)

  combs <- do.call(expand.grid, rep(list(c(FALSE, TRUE)), ncol(regressors)))[-1, ]

  results$results <- apply(combs, 1, function(x){fun.svd_1(y = y, x = regressors[, x, drop = FALSE])})

  cv <- unlist(lapply(results$results, function(x){x$CV}))
  cv.min <- which.min(cv)

  results$results <- results$results[[cv.min]]
  results$G <- G
  class(results) <- "combi"

  return(results)
}
