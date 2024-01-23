#' @title Perform svd regression
#'
#' @description Calculates svd regression.
#'
#' @param x matrix containing independent variables in the model.
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
  D[D <= tol] <- 0

  XtX_inv <- Xsvd$v %*% D %*% D %*% t(Xsvd$v)
  C <- XtX_inv %*% crossprod(x, y)
  rownames(C) <- c("Ind", nombres)

  RSS <- sum((x %*% C - y)^2)
  n <- nrow(x)
  sigma2 <- RSS / n
  rank <- ncol(x)
  k <- rank + 1

  logL <- -(n / 2) * (log(RSS / n)) - (n / 2) * (log(2 * pi)) - (n / 2)
  covmat <- sigma2 * XtX_inv

  lambdas <- eigen(covmat, only.values = TRUE)$values
  lambda_m <- mean(lambdas)
  complexity <- (1 / 2) / (lambda_m * lambda_m) * sum((lambdas - lambda_m)^2)
  CV <- -2 * logL + complexity
  CV <- round(CV, digits = 6)

  resultado$coef <- C
  resultado$CV <- CV
  class(resultado) <- "svd"
  rm(list = c("nombres", "x", "y", "Xsvd", "C", "RSS", "CV", "logL", "XtX_inv", "covmat"))

  return(resultado)
}
