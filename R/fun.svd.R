#' @title Perform svd regression
#'
#' @description Calculates svd regression.
#'
#' @param X matrix containing independent variables in the model.
#' @param y vector or matrix containing dependent variable in the model.
#'
#' @keywords internal
#'

fun.svd <- function(x, y) {

  nombres <- colnames(x)
  resultado <- vector(mode = "list", length = 2)
  names(resultado) <- c("coef", "CV")
  x <- cbind(1, x)

  Xsvd <- svd(x)
  C <- Xsvd$v %*% (crossprod(Xsvd$u, y) / Xsvd$d)
  err <- (x %*% C) - y
  rownames(C) <- c("Ind", nombres)
  CV <- sqrt(mean((err / (1 - rowSums(Xsvd$u * Xsvd$u)))^2))

  resultado$coef <- C
  resultado$CV <- CV
  class(resultado) <- "svd"
  return(resultado)
}
