#' @title GMDH GIA auxiliar functions
#'
#' @description Performs auxiliar tasks to predict.mia
#'
#' @keywords internal
#'
gmdh.gia_2 <- function(X, y, prune, x.test, y.test) {

  ifelse(is.matrix(X), NA, return(message("X must be a matrix")))
  ifelse(ncol(X) <= 2, return(message("GMDH GIA needs more than two regressors")), NA)
  ifelse(prune <= ncol(X), prune <- ncol(X), prune <- prune - ncol(X))
  try(na.fail(X))
  try(na.fail(y))

  inicial <- X
  inicial.test <- x.test
  fin.1 <- Inf
  fin.2 <- 0
  lap <- 0
  modelos <- vector(mode = "list")

  var.inicial <- combn(c(1:ncol(X)), 2, simplify = TRUE)

  while(fin.1 >= fin.2) {

    lap <- lap + 1
    message(paste("Layer ", lap, sep = ""))

    ifelse(lap == 1, var <- var.inicial,
           var <- combn(c(1:ncol(X)), 2, simplify = TRUE)
    )

    ifelse(lap == 1, NA, var.ndx <- which(var[2, ] <= ncol(inicial)))
    ifelse(lap == 1, NA, var <- var[, -var.ndx])

    message(paste(".................... ", ncol(var), " neurons ", sep = ""))

    mod <- apply(var, 2, function(var){gmdh.combi_2(y = y,
                                                    X = cbind(X[, var[1], drop = FALSE],
                                                              X[, var[2], drop = FALSE]),
                                                    x.test = cbind(x.test[, var[1], drop = FALSE],
                                                                   x.test[, var[2], drop = FALSE]),
                                                    y.test = y.test)})

    names(mod) <- paste(lap, c("."), 1:length(mod), sep = "")
    CV.all <- unlist(lapply(mod, function(mod){mod$results$CV}))
    combos <- !duplicated(CV.all, MARGIN = 2)
    CV.all <- CV.all[combos, drop = FALSE]
    ndx <- sort(na.omit(order(CV.all)[1:prune]))
    CV.all <- CV.all[ndx, drop = FALSE]
    mod <- mod[names(CV.all)]

    Z <- lapply(mod, predict.combi, X)
    Z <- matrix(data = unlist(Z), nrow = nrow(X), ncol = length(mod))
    Z.test <- lapply(mod, predict.combi, x.test)
    Z.test <- matrix(data = unlist(Z.test), nrow = nrow(inicial.test), ncol = length(Z.test))
    colnames(Z.test) <- colnames(Z) <- names(mod)
    Z <- cbind(inicial, Z)
    Z.test <- cbind(inicial.test, Z.test)

    fin.1 <- min(CV.all, na.rm = TRUE)

    lap <- lap + 1
    message(paste("Layer ", lap, sep = ""))

    var <- combn(c(1:ncol(Z)), 2, simplify = TRUE)
    var.ndx <- which(var[2, ] <= ncol(inicial))
    var <- var[, -var.ndx]

    message(paste(".................... ", ncol(var), " neurons ", sep = ""))

    mod.Z <- apply(var, 2, function(var){gmdh.combi_2(y = y,
                                                      X = cbind(Z[, var[1], drop = FALSE],
                                                                Z[, var[2], drop = FALSE]),
                                                      x.test = cbind(Z.test[, var[1], drop = FALSE],
                                                                     Z.test[, var[2], drop = FALSE]),
                                                      y.test = y.test)})

    names(mod.Z) <- paste(lap, c("."), 1:length(mod.Z), sep = "")
    CV.all <- unlist(lapply(mod.Z, function(mod.Z){mod.Z$results$CV}))
    combos <- !duplicated(CV.all, MARGIN = 2)
    CV.all <- CV.all[combos, drop = FALSE]
    ndx <- sort(na.omit(order(CV.all)[1:prune]))
    CV.all <- CV.all[ndx, drop = FALSE]
    mod.Z <- mod.Z[names(CV.all)]

    modelos[[length(modelos) + 1]] <- mod
    modelos[[length(modelos) + 1]] <- mod.Z
    class(modelos) <- "gia"

    fin.2 <- min(CV.all, na.rm = TRUE)

    ifelse(fin.2 >= fin.1, return(modelos), NA)

    Z <- lapply(mod.Z, predict.combi, Z)
    X <- matrix(data = unlist(Z), ncol = length(mod.Z))
    x.test <- lapply(mod.Z, predict.combi, Z.test)
    x.test <- matrix(data = unlist(x.test), nrow = nrow(Z.test), ncol = length(x.test))
    colnames(x.test) <- colnames(X) <- names(mod.Z)
    X <- cbind(inicial, X)
    x.test <- cbind(inicial.test, x.test)

  }
}
