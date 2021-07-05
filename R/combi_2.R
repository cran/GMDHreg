#' @title GMDH COMBI auxiliar functions
#'
#' @description Build a regression model performing GMDH Combinatorial with external criteria.
#'
#' @keywords internal
#'
gmdh.combi_2 <- function(X, y, G = 2, x.test, y.test) {

  results <- vector(mode = "list", length = 2)
  names(results) <- c("results", "G")
  regressors <- fun.poly(X, G = G)
  regressors <- fun.filter(regressors)

  x.test <- fun.poly(x.test, G = G)
  x.test <- x.test[, colnames(regressors), drop = FALSE]

  combs <- do.call(expand.grid, rep(list(c(FALSE, TRUE)), ncol(regressors)))[-1, ]

  results$results <- apply(combs, 1, function(x){fun.svd_2(y = y,
                                                           x = regressors[, x, drop = FALSE],
                                                           x.test = x.test[, x, drop = FALSE],
                                                           y.test = y.test)})

  cv <- unlist(lapply(results$results, function(x){x$CV}))
  cv.min <- which.min(cv)

  results$results <- results$results[[cv.min]]
  results$G <- G
  class(results) <- "combi"

  return(results)
}
