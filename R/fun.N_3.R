#' @title GMDH MIA auxiliar functions
#'
#' @description Performs auxiliar tasks to predict.mia
#'
#' @keywords internal
#'
fun.N_3 <- function(x, y) {

  nombres <- colnames(x)
  resultado <- vector(mode = "list", length = 2)
  names(resultado) <- c("coef", "CV")
  tol <- sqrt(.Machine$double.eps)

  x <- cbind(1, x[, 1], x[, 2],
             I(x[, 1]^2), I(x[, 2]^2),
             x[, 1] * x[, 2])

  Xsvd <- svd(x)
  D <- diag(1 / Xsvd$d)
  D[which(D < tol)] <- 0
  XtX_inv <- Xsvd$v %*% D %*% D %*% t(Xsvd$v)
  C <- XtX_inv %*% crossprod(x, y)
  # C <- Xsvd$v %*% (crossprod(Xsvd$u, y) / Xsvd$d)
  rownames(C) <- c("Ind", nombres, paste0(nombres, "^2", sep = ""), "interac")

  ssr <- sum((x %*% C - y)^2)
  logL <- -(nrow(x) / 2) * (log(ssr / nrow(x))) - (nrow(x) / 2) * (log(2 * pi)) - (nrow(x) / 2)
  covmat <- XtX_inv * ssr / (nrow(x) - ncol(x) + 1 - 1)

  # k <- nrow(covmat)
  # CV <- as.vector(-2 * c(logL) + k * log(sum(diag(covmat)) / k) - log(det(covmat))) # ICOMP(IFIM)

  lambdas <- eigen(covmat, only.values = TRUE)$values
  lambda_bar <- mean(lambdas)
  complexity <- .5 / (lambda_bar * lambda_bar) * sum((lambdas - lambda_bar)^2)
  CV <- -2 * logL + complexity

  resultado$coef <- C
  resultado$CV <- CV
  class(resultado) <- "neurona"
  return(resultado)
}
