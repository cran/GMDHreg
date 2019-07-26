#' @title Predict svd regression
#'
#' @description Calculates svd regression model predictions for new data.
#'
#' @param object An object model from fun.svd an object class 'svd'
#' @param newdata matrix containing dependent variable in the model, wich the predictions are calculated.

#'
#' @return NULL
#'
#' @keywords internal
#'
predict.svd <- function(object, newdata, ...) {

  regressors.fin <- rownames(object$coef)
  newdata <- cbind("Ind" = 1, newdata)[, regressors.fin]

  return(newdata %*% object$coef)
}
