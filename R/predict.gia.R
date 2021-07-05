#' @title Predict GMDH GIA object
#'
#' @description Calculates GMDH GIA Twice model predictions for new data.
#'
#' @param object an object of class 'giatwice'
#' @param newdata matrix containing dependent variables in the model, wich the predictions are calculated.
#' @param ...	other undocumented arguments
#'
#' @return A matrix with predictions.
#'
#' @examples
#'
#' set.seed(123)
#' x <- matrix(data = c(rnorm(500)), ncol = 4, nrow = 125)
#' colnames(x) <- c("a", "b", "c", "d")

#' y <- matrix(data = c(10 + x[, "a"] + x[, "d"]^2), ncol = 1)
#' colnames(y) <- "y"
#' x.test <- x[1:5, ]
#' y.test <- y[1:5]
#' x <- x[-c(1:5), ]
#' y <- y[-c(1:5)]
#'
#' mod <- gmdh.gia(X = x, y = y, criteria = "PRESS")
#' pred <- predict(mod, x.test)
#' summary(sqrt((pred - y.test)^2))
#'
#' @export
#'
predict.gia <- function(object, newdata, ...) {

  try(na.fail(newdata))

  object <- lapply(object, function(x){lapply(x, function(x){x$results})})
  n <- length(object) - 1
  inicial <- newdata

  for (i in 1:n) {

    message(paste("Estimando capa ", i, sep = ""))

    datos <- vector(mode = "list", length = length(object[[i]]))
    ifelse(i == 1,
           newdata <- fun.poly(inicial, G = 2),
           newdata <- fun.poly(newdata, G = 2)
    )

    for(j in 1:length(datos)) datos[[j]] <- predict.svd(object[[i]][[j]], newdata)
    newdata <- matrix(data = unlist(datos), ncol = length(datos))
    colnames(newdata) <- names(object[[i]])
    newdata <- cbind(inicial, newdata)
    rm(datos)
  }

  CV <- which.min(unlist(lapply(object[[i]], function(x){x$CV})))
  newdata <- fun.poly(newdata, G = 2)

  return(predict(object[[i + 1]][[CV]], newdata))

}
