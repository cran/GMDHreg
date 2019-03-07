#' @title GMDH MIA auxiliar functions
#'
#' @description Performs auxiliar tasks to predict.mia
#'
#' @keywords internal
#'

fun.N <- function(x, y) {

  nombres <- colnames(x)
  resultado <- vector(mode = "list", length = 2)
  names(resultado) <- c("coef", "CV")

  x <- cbind(1, x[, 1], x[, 2],
             I(x[, 1]^2), I(x[, 2]^2),
             x[, 1] * x[, 2])

  Xsvd <- svd(x)

  C <- Xsvd$v %*% (crossprod(Xsvd$u, y) / Xsvd$d)
  err <- (x %*% C) - y
  rownames(C) <- c("Ind", nombres, paste0(nombres, "^2", sep = ""), "interac")

  CV <- sqrt(mean((err / (1 - rowSums(Xsvd$u * Xsvd$u)))^2))

  resultado$coef <- C
  resultado$CV <- CV
  class(resultado) <- "neurona"
  return(resultado)
}
