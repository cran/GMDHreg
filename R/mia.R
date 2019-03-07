#' @title GMDH MIA with PRESS like external criteria.
#'
#' @description Performs GMDH MIA with PRESS like selection criteria.
#'
#' @param X matrix with N columns and M rows, containing independent variables in the model.
#' @param y vector or matrix containing dependent variable in the model.
#' @param prune an integer whose recommended minimum value is the number of initial regressors. \cr
#'    The maximum value will depend on the available RAM. It is recommended to work with the maximum value, but it can be computationally very expensive. \cr
#'    prune is the selected number of neurons from layer i to layer i+1. The resulting layer i+1 has prune(prune-1)/2 neurons; for example with prune=150, the resulting nerurons will be 11.175
#'
#' @return An object of class mia.
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
#' @references
#' Farlow, S.J. (1981): "The GMDH algorithm of Ivakhnenko", The American Statistician, 35(4), pp. 210-215. <doi:10.2307/2683292> \cr
#' Ivakhnenko A.G. (1968): "The Group Method of Data Handling - A Rival of the Method of Stochastic Approximation", Soviet Automatic Control, 13(3), pp. 43-55
#'
#' @importFrom stats na.omit predict
#'
#' @importFrom utils combn
#'
#' @export
#'

gmdh.mia <- function(X, y, prune = 150) {

  ifelse(is.matrix(X), NA, return(message("X must be a matrix")))
  ifelse(ncol(X) <= 4, return(message("Please use GMDH Combinatorial")), NA)

  fin.1 <- Inf
  fin.2 <- 0
  lap <- 0
  modelos <- vector(mode = "list")

  while(fin.1 >= fin.2) {

    lap <- lap + 1
    message(paste("Layer ", lap, sep = ""))

    var <- combn(c(1:ncol(X)), 2, simplify = TRUE)
    message(paste(".................... ", ncol(var), " neurons ", sep = ""))

    mod <- apply(var, 2, function(var){fun.N(y = y,
                                             x = cbind(X[, var[1], drop = FALSE],
                                                       X[, var[2], drop = FALSE]))})

    Z <- lapply(mod, function(mod){predict(mod, X)})
    Z <- matrix(data = unlist(Z), nrow = nrow(X), ncol = length(mod))
    colnames(Z) <- Z.nombres <- names(mod) <- nombres.mod <- paste(lap, c("."), 1:length(mod), sep = "")

    combos <- !duplicated(Z, MARGIN = 2)
    Z <- Z[, combos, drop = FALSE]
    mod <- mod[combos]
    Z.nombres <- names(mod) <- colnames(Z)

    CV <- unlist(lapply(mod, function(x){x$CV}))
    ndx <- sort(na.omit(order(CV)[1:prune]))
    Z <- Z[, ndx, drop = FALSE]
    Z.nombres <- colnames(Z)

    fin.1 <- min(CV, na.rm = TRUE)

    lap <- lap + 1
    message(paste("Layer ", lap, sep = ""))

    var <- combn(c(1:ncol(Z)), 2, simplify = TRUE)
    message(paste(".................... ", ncol(var), " neurons ", sep = ""))

    mod.Z <- apply(var, 2, function(var){fun.N(y = y,
                                               x = cbind(Z[, var[1], drop = FALSE],
                                                         Z[, var[2], drop = FALSE]))})

    nombres.mod <- names(mod.Z) <- paste(lap, c("."), 1:length(mod.Z), sep = "")

    modelos[[length(modelos) + 1]] <- mod
    modelos[[length(modelos) + 1]] <- mod.Z

    CV <- unlist(lapply(mod.Z, function(x){x$CV}))
    fin.2 <- min(CV, na.rm = TRUE)
    model <- modelos[1:(length(modelos) - 1)]
    class(model) <- "mia"

    ifelse(fin.2 >= fin.1, return(model), NA)

    X <- lapply(mod.Z, function(mod.Z){predict(mod.Z, Z)})
    X <- matrix(data = unlist(X), ncol = length(mod.Z))
    colnames(X) <- nombres.mod

    combos <- !duplicated(X, MARGIN = 2)
    X <- X[, combos, drop = FALSE]
    mod.Z <- mod.Z[combos]
    nombres.mod <- nombres.mod[combos]
    CV <- unlist(lapply(mod.Z, function(x){x$CV}))
    ndx <- sort(na.omit(order(CV)[1:prune]))
    X <- X[, ndx, drop = FALSE]

  }
}
