#' @title GMDH Combinatorial
#'
#' @description Performs GMDH Combinatorial with PRESS like selection criteria. \cr
#' Be careful with input matrix, more than four regressors could be computationally very expensive and time consuming.
#'
#' @param X matrix with N columns and M rows, containing independent variables in the model. N must be greater than 1.
#' @param y vector or matrix containing dependent variable in the model.
#' @param G polynomial degree. Default is 2, original Ivakhnenko quadratic polynomial. \cr
#'     0: linear regression without quadratic and interactrion terms. \cr
#'     1: only interaction terms. \cr
#'
#' @return An object of class 'combi'. This is a list with two elements: results and G \cr
#'     results is a list with two elements: \cr
#'     \itemize{
#'       \item coef: coeficients of final selected GMDH Combinatorial model.
#'       \item CV: PRESS criteria value for selected model.
#'       }
#' @return G the grade of polynomial used in GMDH Combinatorial model.
#'
#' @examples
#' set.seed(123)
#' x <- matrix(data = c(rnorm(1050)), ncol = 3, nrow = 350)
#' colnames(x) <- c("a", "b", "c")

#' y <- matrix(data = c(10 + x[, "a"] + x[, "b"]^2 + x[, "c"]^3), ncol = 1)
#' colnames(y) <- "y"
#' x.test <- x[1:10, ]
#' y.test <- y[1:10]
#' x <- x[-c(1:10), ]
#' y <- y[-c(1:10)]
#'
#' mod <- gmdh.combi(X = x, y = y)
#' pred <- predict(mod, x.test)
#' summary(sqrt((pred - y.test)^2))
#'
#' @references
#' Ivakhnenko A.G. (1968): "The Group Method of Data Handling - A Rival of the Method of Stochastic Approximation", Soviet Automatic Control, 13(3), pp. 43-55
#'
#' @importFrom stats na.omit predict
#'
#' @export
#'

gmdh.combi <- function(X, y, G = 2) {

  ifelse(is.matrix(X), NA, return(message("X must be a matrix")))
  ifelse(ncol(X) <= 4, NA, return(message("Warming more than 4 regressors")))
  ifelse(ncol(X) >= 2, NA, return(message("GMDH Combinatorial needs more than 1 regressor")))

  results <- vector(mode = "list", length = 2)
  names(results) <- c("results", "G")

  ifelse(ncol(X) == 1, return(fun.svd(x = X, y = y)),
                       regressors <- fun.poly(X, G = G)
  )

  combs <- do.call(expand.grid, rep(list(c(FALSE, TRUE)), ncol(regressors)))[-1, ]

  results$results <- apply(combs, 1, function(x){fun.svd(y = y, x = regressors[, x, drop = FALSE])})

  cv <- unlist(lapply(results$results, function(x){x$CV}))
  cv.min <- which.min(cv)

  results$results <- results$results[[cv.min]]
  results$G <- G
  class(results) <- "combi"

  return(results)
}
