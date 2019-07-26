#' @title Perform svd regression
#'
#' @description Calculates svd regression.
#'
#' @param X matrix containing independent variables in the model.
#' @param y vector or matrix containing dependent variable in the model.
#'
#' @keywords internal
#'
fun.svd_3 <- function(x, y) {

  nombres <- colnames(x)
  resultado <- vector(mode = "list", length = 2)
  names(resultado) <- c("coef", "CV")
  x <- cbind(1, x)
  tol <- sqrt(.Machine$double.eps)

  Xsvd <- svd(x)
  D <- diag(1 / Xsvd$d)
  D[which(D < tol)] <- 0
  XtX_inv <- Xsvd$v %*% D %*% D %*% t(Xsvd$v)
  C <- XtX_inv %*% crossprod(x, y)
  # C <- Xsvd$v %*% (crossprod(Xsvd$u, y) / Xsvd$d)
  rownames(C) <- c("Ind", nombres)

  ssr <- sum((x %*% C - y)^2)
  logL <- -(nrow(x) / 2) * (log(ssr / nrow(x))) - (nrow(x) / 2) * (log(2 * pi)) - (nrow(x) / 2)
  covmat <- XtX_inv * ssr / (nrow(x) - ncol(x) + 1 - 1)

  # k <- nrow(covmat)
  # CV <- as.vector(-2 * c(logL) + k * log(sum(diag(covmat)) / k) - log(det(covmat))) # ICOMP(IFIM)

  lambdas <- eigen(covmat, only.values = TRUE)$values
  lambda_bar <- mean(lambdas)
  complexity <- .5 / (lambda_bar * lambda_bar) * sum((lambdas - lambda_bar)^2)
  CV <- -2 * logL + complexity # ICOMP

  resultado$coef <- C
  resultado$CV <- CV
  class(resultado) <- "svd"
  rm(list = c("nombres", "x", "y", "Xsvd", "C", "ssr", "CV", "logL", "XtX_inv", "covmat"))

  return(resultado)
}