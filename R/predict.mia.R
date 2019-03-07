#' @title Predict GMDH MIA object
#'
#' @description Calculates GMDH MIA model predictions for new data.
#'
#' @param object an object of class 'mia'
#' @param newdata matrix containing dependent variables in the model, wich the predictions are calculated.
#' @param ...	other undocumented arguments
#'
#' @return A matrix with predictions.
#'
#' @examples
#' set.seed(123)
#' x <- matrix(data = c(rnorm(1000)), ncol = 5, nrow = 200)
#' colnames(x) <- c("a", "b", "c", "d", "e")

#' y <- matrix(data = c(10 + x[, "a"] * x[, "e"]^3), ncol = 1)
#' colnames(y) <- "y"
#' x.test <- x[1:10, ]
#' y.test <- y[1:10]
#' x <- x[-c(1:10), ]
#' y <- y[-c(1:10)]
#'
#' mod <- gmdh.mia(X = x, y = y, prune = 5)
#' pred <- predict(mod, x.test)
#' summary(sqrt((pred - y.test)^2))
#'
#' @export
#'

predict.mia <- function(object, newdata, ...) {

  lon <- length(object)

  ifelse(lon == 1,
         return(fun.aux.1(object = object, newdata = newdata)),
         return(fun.aux.2(object = object, newdata = newdata, lon = lon))
  )
}
