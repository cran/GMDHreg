#' @title Auxiliar function to remove duplicated or zero variance regressors
#'
#' @description Auxiliar function to remove duplicated regressors.
#'
#' @importFrom stats var
#'
#' @keywords internal
#'
fun.filter <- function(X){

  tol <- sqrt(.Machine$double.eps)

  dup <- !duplicated(X, MARGIN = 2)
  X <- X[, dup, drop = FALSE]

  .var <- apply(X, 2, var)
  .var <- .var <= tol
  X <- X[, !.var, drop = FALSE]

  return(X)

  }
