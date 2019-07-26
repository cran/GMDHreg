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
#' mod <- gmdh.mia(X = x, y = y, prune = 5, criteria = "PRESS")
#' pred <- predict(mod, x.test)
#' summary(sqrt((pred - y.test)^2))
#'
#' @export
#'
predict.mia <- function(object, newdata, ...) {

  try(na.fail(newdata))

  resultado <- vector(mode = "list", length = (length(object) - 1))
  resultado[[1]] <- newdata
  rm(newdata)

  for (i in 1:length(resultado)) {

    message(paste("Estimando capa ", i, sep = ""))

    newdata <- vector(mode = "list", length = length(object[[i]]))
    newdata <- lapply(newdata, function(x){x <- matrix(data = NA, ncol = 1, nrow = nrow(resultado[[1]]))})
    data <- resultado[[i]]
    for(j in 1:length(newdata)) newdata[[j]] <- predict.neurona(object[[i]][[j]], data)
    newdata <- matrix(data = unlist(newdata), ncol = length(newdata))
    colnames(newdata) <- names(object[[i]])
    resultado[[(i + 1)]] <- newdata
    rm(newdata)
    rm(data)
  }

  CV <- which.min(unlist(lapply(object[[i]], function(x){x$CV})))

  return(predict(object[[i]][[CV]], resultado[[i]]))

}
