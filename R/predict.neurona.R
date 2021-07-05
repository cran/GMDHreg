#' @title Predict neurona object
#'
#' @description Calculates neurona model predictions for new data.
#'
#' @param object An object model from mia an object class 'neurona'
#' @param newdata matrix containing dependent variable in the model, wich the predictions are calculated.
#'
#' @return NULL
#'
#' @keywords internal
#'
predict.neurona <- function(object, newdata, ...) {

  regressors.fin <- rownames(object$coef)[c(2, 3)]
  newdata <- newdata[, regressors.fin]

  newdata <- cbind(1, newdata[, 1], newdata[, 2],
                   I(newdata[, 1]^2), I(newdata[, 2]^2),
                   newdata[, 1] * newdata[, 2])

  return(newdata %*% object$coef)
}
