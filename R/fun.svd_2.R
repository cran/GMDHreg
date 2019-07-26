#' @title Perform svd regression
#'
#' @description Calculates svd regression.
#'
#' @param X matrix containing independent variables in the model.
#' @param y vector or matrix containing dependent variable in the model.
#'
#' @keywords internal
#'
fun.svd_2 <- function(x, y, x.test, y.test) {

  nombres <- colnames(x)
  resultado <- vector(mode = "list", length = 2)
  names(resultado) <- c("coef", "CV")
  x <- cbind(1, x)
  x.test <- cbind(1, x.test)
  tol <- sqrt(.Machine$double.eps)

  Xsvd <- svd(x)
  D <- 1 / Xsvd$d
  D[which(D < tol)] <- 0
  C <- Xsvd$v %*% (crossprod(Xsvd$u, y) * D)
  rownames(C) <- c("Ind", nombres)

  err <- x.test %*% C - y.test
  CV <- sqrt(mean(err^2))

  resultado$coef <- C
  resultado$CV <- CV
  class(resultado) <- "svd"
  rm(list = c("nombres", "x", "y", "x.test", "y.test", "Xsvd", "C", "err", "CV"))

  return(resultado)
}
