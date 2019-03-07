#' @title GMDH MIA auxiliar functions
#'
#' @description Performs auxiliar tasks to predict.mia
#'
#' @examples NULL
#'
#' @keywords internal
#' @export
#'

predict.neurona <- function(object, newdata, ...) {

  regressors.fin <- rownames(object$coef)[c(2, 3)]
  newdata <- newdata[, regressors.fin]

  newdata <- cbind(1, newdata[, 1], newdata[, 2],
                   I(newdata[, 1]^2), I(newdata[, 2]^2),
                   newdata[, 1] * newdata[, 2])

  return(newdata %*% object$coef)
}
