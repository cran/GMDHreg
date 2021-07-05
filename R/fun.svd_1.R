#' @title Perform svd regression
#'
#' @description Calculates svd regression.
#'
#' @param X matrix containing independent variables in the model.
#' @param y vector or matrix containing dependent variable in the model.
#'
#' @keywords internal
#'
fun.svd_1 <- function(x, y) {

  nombres <- colnames(x)
  resultado <- vector(mode = "list", length = 2)
  names(resultado) <- c("coef", "CV")
  x <- cbind(1, x)
  tol <- sqrt(.Machine$double.eps)

  Xsvd <- svd(x)
  D <- 1 / Xsvd$d
  D[D <= tol] <- 0
  C <- Xsvd$v %*% (crossprod(Xsvd$u, y) * D)
  rownames(C) <- c("Ind", nombres)

  err <- (x %*% C) - y
  CV <- sqrt(mean((err / (1 - rowSums(Xsvd$u * Xsvd$u)))^2))
  CV <- round(CV, digits = 6)

  resultado$coef <- C
  resultado$CV <- CV
  class(resultado) <- "svd"
  rm(list = c("nombres", "x", "y", "Xsvd", "C", "err", "CV"))

  return(resultado)
}
