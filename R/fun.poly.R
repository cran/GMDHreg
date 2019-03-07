#' @title Performs polynomial to use in GMDH Combinatorial.
#'
#' @description Performs polynomial to use in GMDH Combinatorial.
#'
#' @param X matrix containing independent variables in the model.
#' @param G polynomial. Default is 2, original Ivakhnenko polynomial.
#'     0: normal regression.
#'     1: only interaction terms.
#'
#' @return A matrix with n columns corresponding with Ivakhnenko polynomial
#'
#' @keywords internal
#'

fun.poly <- function(X, G = 2) {

  ifelse(G == 0, return(X), NA)

  nombres.or <- colnames(X)
  nombres.par <- combn(nombres.or, 2)
  nombres.par <- apply(nombres.par, 2, function(x){paste(x[1], "*", x[2], sep = "")})

  var <- combn(c(1:ncol(X)), 2)
  pares <- apply(var, 2, function(x){I(X[,x[1]]) * I(X[,x[2]])})
  colnames(pares) <- nombres.par

  ifelse(G == 1, return(cbind(X, pares)), NA)

  X.sq <- matrix(data = I(X^2), nrow = nrow(X), ncol = ncol(X))
  colnames(X.sq) <- paste(nombres.or, "^", "2", sep = "")
  BD <- cbind(X, X.sq, pares)

  ifelse(G == 2, return(BD), NA)

}
