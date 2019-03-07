#' @title GMDH MIA auxiliar functions
#'
#' @description Performs auxiliar tasks to predict.mia
#'
#' @keywords internal
#'

fun.aux.1 <- function(object, newdata, ...) {

  CV <- which.min(unlist(lapply(object[[1]], function(x){x$CV})))
  Best <- object[[1]][[CV]]
  return(predict(Best, newdata))
}

fun.aux.2 <- function(object, newdata, lon, ...){

  j <- 1

  while(j <= (lon - 1)) {

    message(paste("Layer ", j, sep = ""))
    newdata <- lapply(object[[j]], predict, newdata)
    newdata <- matrix(data = unlist(newdata), ncol = length(newdata))
    colnames(newdata) <- names(object[[j]])
    j <- j + 1
  }

  message(paste("Layer ", j, sep = ""))
  CV <- which.min(unlist(lapply(object[[lon]], function(x){x$CV})))
  return(predict(object[[lon]][[CV]], newdata))
}
