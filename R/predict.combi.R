#' @title Predict GMDH Combinatorial
#'
#' @description Calculates GMDH Combinatorial model predictions for new data.
#'
#' @param object an object of class 'combi'
#' @param newdata matrix containing dependent variables in the model, wich the predictions are calculated.
#' @param ...	other undocumented arguments
#'
#' @return A matrix with predictions.
#'
#' @examples
#' set.seed(123)
#' x <- matrix(data = c(rnorm(1050)), ncol = 3, nrow = 350)
#' colnames(x) <- c("a", "b", "c")

#' y <- matrix(data = c(10 + x[, "a"] + x[, "b"]^2 + x[, "c"]^3), ncol = 1)
#' colnames(y) <- "y"
#' x.test <- x[1:10, ]
#' y.test <- y[1:10]
#' x <- x[-c(1:10), ]
#' y <- y[-c(1:10)]
#'
#' mod <- gmdh.combi(X = x, y = y, criteria = "PRESS")
#' pred <- predict(mod, x.test)
#' summary(sqrt((pred - y.test)^2))
#'
#' @export
#'
predict.combi <- function(object, newdata, ...) {

  try(na.fail(newdata))

  regressors.fin <- rownames(object$results$coef)
  newdata <- cbind(Ind = 1, fun.poly(newdata, G = object$G))[, regressors.fin]

  return(newdata %*% object$results$coef)
}
