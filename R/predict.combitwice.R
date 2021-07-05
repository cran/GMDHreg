#' @title Predict GMDH Twice-Multilayered Combinatorial
#'
#' @description Calculates GMDH Twice-Multilayered Combinatorial model predictions for new data.
#'
#' @param object an object of class 'combitwice'
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
#' mod <- gmdh.combi.twice(X = x, y = y, criteria = "PRESS")
#' pred <- predict(mod, x.test)
#' summary(sqrt((pred - y.test)^2))
#'
#' @export
#'
predict.combitwice <- function(object, newdata, ...) {

  try(na.fail(newdata))

  G <- object$G
  object <- object$results
  n <- length(object) - 2

  for (i in 1:n) {

    message(paste("Estimando capa ", i, sep = ""))

    datos <- vector(mode = "list", length = length(object[[i]]))
    newdata <- fun.poly(newdata, G = G)
    for(j in 1:length(datos)) datos[[j]] <- predict.svd(object[[i]][[j]], newdata)
    newdata <- matrix(data = unlist(datos), ncol = length(datos))
    colnames(newdata) <- names(object[[i]])
    rm(datos)
  }

  message(paste("Estimando capa ", i + 1, sep = ""))
  CV <- which.min(unlist(lapply(object[[i]], function(x){x$CV})))
  newdata <- fun.poly(newdata, G = G)

  return(predict(object[[i + 1]][[CV]], newdata))

}
