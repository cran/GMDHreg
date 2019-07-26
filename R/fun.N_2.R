#' @title GMDH MIA auxiliar functions
#'
#' @description Performs auxiliar tasks to predict.mia
#'
#' @keywords internal
#'
fun.N_2 <- function(x, y, x.test, y.test) {

  nombres <- colnames(x)
  resultado <- vector(mode = "list", length = 2)
  names(resultado) <- c("coef", "CV")
  tol <- sqrt(.Machine$double.eps)

  x <- cbind(1, x[, 1], x[, 2],
             I(x[, 1]^2), I(x[, 2]^2),
             x[, 1] * x[, 2])

  x.test <- cbind(1, x.test[, 1], x.test[, 2],
                  I(x.test[, 1]^2), I(x.test[, 2]^2),
                  x.test[, 1] * x.test[, 2])

  Xsvd <- svd(x)
  D <- 1 / Xsvd$d
  D[which(D < tol)] <- 0

  C <- Xsvd$v %*% (crossprod(Xsvd$u, y) * D)
  err <- x.test %*% C - y.test
  rownames(C) <- c("Ind", nombres, paste0(nombres, "^2", sep = ""), "interac")

  CV <- sqrt(mean(err^2))

  resultado$coef <- C
  resultado$CV <- CV
  class(resultado) <- "neurona"
  return(resultado)
}
