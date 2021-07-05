#' @title GMDH GIA
#'
#' @description Build a regression model performing GMDH GIA (Generalized Iterative Algorithm) with Active Neurons (Combinatorial algorithm). \cr
#' For more information, please read the package's vignette.
#'
#' @param X matrix with N>3 columns and M rows, containing independent variables in the model. \cr
#' The data must not contain NAs
#' @param y vector or matrix containing dependent variable in the model. \cr
#' The data must not contain NAs
#' @param prune an integer whose recommended minimum value is the number of initial regressors. \cr
#'    The maximum value will depend on the available RAM. \cr
#'    Prune is the selected number of neurons from layer i to layer i+1. The resulting layer i+1 has prune(prune-1)/2 neurons; for example with prune=150, the resulting nerurons will be 11.175
#' @param criteria GMDH external criteria. Values: \cr
#'      \itemize{
#'       \item PRESS: predicted residual error sum of squares. \cr
#'       \item test: use x.test and y.test to estimate RMSE (root mean squeare errors). \cr
#'       \item ICOMP: Index of Informational Complexity. Like PRESS, it is computed without recalculating of system.
#'      }
#' @param x.test matrix with a sample randomly drawn from the initial data. \cr
#'    It is used when criteria = test. \cr
#'    This sample should not be included in X.
#' @param y.test vector or matrix with y values correspond with x.test values.
#'
#' @return An object of class gia.
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
#' @references
#' Bozdogan, H. and Haughton, D.M.A. (1998): "Information complexity criteria for regression models", Computational Statistics & Data Analysis, 28, pp. 51-76 <doi: 10.1016/S0167-9473(98)00025-5> \cr
#'
#' Farlow, S.J. (1981): "The GMDH algorithm of Ivakhnenko", The American Statistician, 35(4), pp. 210-215. <doi: 10.2307/2683292> \cr
#'
#' Hild, Ch. R. and Bozdogan, H. (1995): "The use of information-based model selection criteria in the GMDH algorithm", Systems Analysis Modelling Simulation, 20(1-2), pp. 29-50 \cr
#'
#' Ivakhnenko, A.G. (1968): "The Group Method of Data Handling - A Rival of the Method of Stochastic Approximation", Soviet Automatic Control, 13(3), pp. 43-55 \cr
#'
#' Müller, J.-A., Ivachnenko, A.G. and Lemke, F. (1998): "GMDH Algorithms for Complex Systems Modelling", Mathematical and Computer Modelling of Dynamical Systems, 4(4), pp. 275-316 <doi: 10.1080/13873959808837083> \cr
#'
#' Stepashko, V. Bulgakova, O. and Zosimov V. (2018): "Construction and Research of the Generalized Iterative GMDH Algorithm with Active Neurons", Advances in Intelligent Systems and Computing II, pp. 492-510 <doi:10.1007/978-3-319-70581-1_35>
#'
#' @importFrom stats na.omit na.fail predict
#'
#' @importFrom utils combn
#'
#' @export
#'
gmdh.gia <- function(X, y, prune = ncol(X), criteria = c("PRESS", "test", "ICOMP"), x.test = NULL, y.test = NULL) {

  if (is.matrix(X) != TRUE)
    stop("X must be a matrix")
  if (is.null(colnames(X)) == TRUE)
    stop("X matrix regressors must have names. Try colnames() function")
  if(ncol(X) <= 2)
    stop("GMDH GIA needs more than two regressors")
  if(any(is.na(X)) != FALSE)
    stop("X has NA values")
  if(any(is.na(y)) != FALSE)
    stop("y has NA values")

  switch(criteria,
         PRESS = return(gmdh.gia_1(X = X, y = y, prune = prune)),
         test = return(gmdh.gia_2(X= X, y = y, prune = prune, x.test = x.test, y.test = y.test)),
         ICOMP = return(gmdh.gia_3(X= X, y = y, prune = prune))
  )
}
