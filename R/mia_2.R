#' @title GMDH MIA auxiliar functions
#'
#' @description Performs auxiliar tasks to predict.mia
#'
#' @keywords internal
#'
gmdh.mia_2 <- function(X, y, prune, x.test, y.test) {

  fin.1 <- Inf
  fin.2 <- 0
  lap <- 0
  modelos <- vector(mode = "list")

  while(fin.1 >= fin.2) {

    lap <- lap + 1
    message(paste("Layer ", lap, sep = ""))

    .var <- combn(c(1:ncol(X)), 2, simplify = TRUE)
    message(paste(".................... ", ncol(.var), " neurons ", sep = ""))

    mod <- apply(.var, 2, function(.var){fun.N_2(y = y,
                                                 x = cbind(X[, .var[1], drop = FALSE],
                                                           X[, .var[2], drop = FALSE]),
                                                 x.test = cbind(x.test[, .var[1], drop = FALSE],
                                                                x.test[, .var[2], drop = FALSE]),
                                                 y.test = y.test)})

    names(mod) <- paste(lap, c("."), 1:length(mod), sep = "")
    CV.all <- unlist(lapply(mod, function(mod){mod$CV}))

    Z <- lapply(mod, predict.neurona, X)
    Z <- matrix(data = unlist(Z), nrow = nrow(X), ncol = length(mod))
    Z.test <- lapply(mod, predict.neurona, x.test)
    Z.test <- matrix(data = unlist(Z.test), nrow = nrow(x.test), ncol = length(Z.test))
    colnames(Z.test) <- colnames(Z) <- names(mod)

    Z <- fun.filter(Z)
    nombres.Z <- colnames(Z)
    Z.test <- Z.test[, nombres.Z]
    CV.all <- CV.all[nombres.Z]

    ndx <- sort(na.omit(order(CV.all)[1:prune]))
    CV.all <- CV.all[ndx, drop = FALSE]
    mod <- mod[names(CV.all)]
    Z <- Z[, names(CV.all)]
    Z.test <- Z.test[, names(CV.all)]

    fin.1 <- min(CV.all, na.rm = TRUE)
    message(paste("          Error ", fin.1, sep = ""))

    lap <- lap + 1
    message(paste("Layer ", lap, sep = ""))

    .var <- combn(c(1:ncol(Z)), 2, simplify = TRUE)
    message(paste(".................... ", ncol(.var), " neurons ", sep = ""))

    mod.Z <- apply(.var, 2, function(.var){fun.N_2(y = y,
                                                   x = cbind(Z[, .var[1], drop = FALSE],
                                                             Z[, .var[2], drop = FALSE]),
                                                   x.test = cbind(Z.test[, .var[1], drop = FALSE],
                                                                Z.test[, .var[2], drop = FALSE]),
                                                   y.test = y.test)})

    names(mod.Z) <- paste(lap, c("."), 1:length(mod.Z), sep = "")
    CV.all <- unlist(lapply(mod.Z, function(mod.Z){mod.Z$CV}))

    X <- lapply(mod.Z, predict.neurona, Z)
    X <- matrix(data = unlist(X), ncol = length(mod.Z))
    x.test <- lapply(mod.Z, predict.neurona, Z.test)
    x.test <- matrix(data = unlist(x.test), nrow = nrow(Z.test), ncol = length(x.test))
    colnames(x.test) <- colnames(X) <- names(mod.Z)

    X <- fun.filter(X)
    nombres.X <- colnames(X)
    CV.all <- CV.all[nombres.X]

    ndx <- sort(na.omit(order(CV.all)[1:prune]))
    CV.all <- CV.all[ndx, drop = FALSE]
    mod.Z <- mod.Z[names(CV.all)]
    X <- X[, names(CV.all)]
    x.test <- x.test[, names(CV.all)]

    modelos[[length(modelos) + 1]] <- mod
    modelos[[length(modelos) + 1]] <- mod.Z
    class(modelos) <- "mia"

    fin.2 <- min(CV.all, na.rm = TRUE)
    message(paste("          Error ", fin.2, sep = ""))

    ifelse(fin.2 >= fin.1, return(modelos), NA)

  }
}
