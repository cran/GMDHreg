#' @title GMDH MIA auxiliar functions
#'
#' @description Performs auxiliar tasks to predict.mia
#'
#' @keywords internal
#'
gmdh.mia_1 <- function(X, y, prune) {

  ifelse(is.matrix(X), NA, return(message("X must be a matrix")))
  ifelse(ncol(X) <= 2, return(message("GMDH MIA needs more than two regressors")), NA)
  ifelse(prune <= ncol(X), prune <- ncol(X), prune <- prune - ncol(X))
  try(na.fail(X))
  try(na.fail(y))

  fin.1 <- Inf
  fin.2 <- 0
  lap <- 0
  modelos <- vector(mode = "list")

  while(fin.1 >= fin.2) {

    lap <- lap + 1
    message(paste("Layer ", lap, sep = ""))

    var <- combn(c(1:ncol(X)), 2, simplify = TRUE)
    message(paste(".................... ", ncol(var), " neurons ", sep = ""))

    mod <- apply(var, 2, function(var){fun.N_1(y = y,
                                               x = cbind(X[, var[1], drop = FALSE],
                                                         X[, var[2], drop = FALSE]))})

    names(mod) <- paste(lap, c("."), 1:length(mod), sep = "")
    CV.all <- unlist(lapply(mod, function(mod){mod$CV}))
    combos <- !duplicated(CV.all, MARGIN = 2)
    CV.all <- CV.all[combos, drop = FALSE]
    ndx <- sort(na.omit(order(CV.all)[1:prune]))
    CV.all <- CV.all[ndx, drop = FALSE]
    mod <- mod[names(CV.all)]

    Z <- lapply(mod, predict, X)
    Z <- matrix(data = unlist(Z), nrow = nrow(X), ncol = length(mod))
    colnames(Z) <- names(mod)

    fin.1 <- min(CV.all, na.rm = TRUE)

    lap <- lap + 1
    message(paste("Layer ", lap, sep = ""))

    var <- combn(c(1:ncol(Z)), 2, simplify = TRUE)
    message(paste(".................... ", ncol(var), " neurons ", sep = ""))

    mod.Z <- apply(var, 2, function(var){fun.N_1(y = y,
                                                 x = cbind(Z[, var[1], drop = FALSE],
                                                           Z[, var[2], drop = FALSE]))})

    names(mod.Z) <- paste(lap, c("."), 1:length(mod.Z), sep = "")
    CV.all <- unlist(lapply(mod.Z, function(mod.Z){mod.Z$CV}))
    combos <- !duplicated(CV.all, MARGIN = 2)
    CV.all <- CV.all[combos, drop = FALSE]
    ndx <- sort(na.omit(order(CV.all)[1:prune]))
    CV.all <- CV.all[ndx, drop = FALSE]
    mod.Z <- mod.Z[names(CV.all)]

    modelos[[length(modelos) + 1]] <- mod
    modelos[[length(modelos) + 1]] <- mod.Z
    class(modelos) <- "mia"

    fin.2 <- min(CV.all, na.rm = TRUE)

    ifelse(fin.2 >= fin.1, return(modelos), NA)

    X <- lapply(mod.Z, predict, Z)
    X <- matrix(data = unlist(X), ncol = length(mod.Z))
    colnames(X) <- names(mod.Z)

  }
}
