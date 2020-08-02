#' @title GMDH MIA
#'
#' @description Build a regression model performing GMDH MIA (Multilayered Iterative Algorithm). \cr
#' For more information, please read the package's vignette.
#'
#' @param X matrix with N>3 columns and M rows, containing independent variables in the model. \cr
#' The data must not contain NAs
#' @param y vector or matrix containing dependent variable in the model. \cr
#' The data must not contain NAs
#' @param prune an integer whose recommended minimum value is the number of initial regressors. \cr
#'    The maximum value will depend on the available RAM. It is recommended to work with the maximum value, but it can be computationally very expensive. \cr
#'    Prune is the selected number of neurons from layer i to layer i+1. The resulting layer i+1 has prune(prune-1)/2 neurons; for example with prune=150, the resulting nerurons will be 11.175
#' @param criteria GMDH external criteria. Values: \cr
#'      \itemize{
#'       \item PRESS: Predicted Residual Error Sum of Squares. It take into account all information in data sample and it is computed without recalculating of system for each test point.\cr
#'       \item test: use x.test and y.test to estimate RMSE (Root Mean Squeare Errors). \cr
#'       \item ICOMP: Index of Informational Complexity. Like PRESS, it is computed without recalculating of system.
#'      }
#' @param x.test matrix with a sample randomly drawn from the initial data. \cr
#'    It is used when criteria = test. \cr
#'    This sample should not be included in X.
#' @param y.test vector or matrix with y values correspond with x.test values.
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
#' mod <- gmdh.mia(X = x, y = y, prune = 5, criteria = "PRESS")
#' pred <- predict(mod, x.test)
#' summary(sqrt((pred - y.test)^2))
#'
#' @references
#' Bozdogan, H. and Haughton, D.M.A. (1998): "Information complexity criteria for regression models", Computational Statistics & Data Analysis, 28, pp. 51-76 <doi: 10.1016/S0167-9473(98)00025-5> \cr
#'
#' Farlow, S.J. (1981): "The GMDH algorithm of Ivakhnenko", The American Statistician, 35(4), pp. 210-215. <doi:10.2307/2683292> \cr
#'
#' Hild, Ch. R. and Bozdogan, H. (1995): "The use of information-based model selection criteria in the GMDH algorithm", Systems Analysis Modelling Simulation, 20(1-2), pp. 29-50 \cr
#'
#' Ivakhnenko A.G. (1968): "The Group Method of Data Handling - A Rival of the Method of Stochastic Approximation", Soviet Automatic Control, 13(3), pp. 43-55
#'
#' @importFrom stats na.omit na.fail predict
#'
#' @importFrom utils combn
#'
#' @export
#'
gmdh.mia <- function(X, y, prune = 150, criteria = c("PRESS", "test", "ICOMP"), x.test = NULL, y.test = NULL) {

  if (is.matrix(X) != TRUE)
    stop("X must be a matrix")
  if(any(is.na(X)) != FALSE)
    stop("X has NA values")
  if(any(is.na(y)) != FALSE)
    stop("y has NA values")

  switch(criteria,
         PRESS = return(gmdh.mia_1(X = X, y = y, prune = prune)),
         test = return(gmdh.mia_2(X= X, y = y, prune = prune, x.test = x.test, y.test = y.test)),
         ICOMP = return(gmdh.mia_3(X= X, y = y, prune = prune))
  )
}
