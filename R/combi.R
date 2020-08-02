#' @title GMDH Combinatorial
#'
#' @description Build a regression model performing GMDH Combinatorial. \cr
#' This is the basic GMDH algorithm. For more information, please read the package's vignette.
#'
#' @param X matrix with N>1 columns and M rows, containing independent variables in the model. \cr
#' Be careful, N>4 and G=2, could be computationally very expensive and time consuming. \cr
#' The data must not contain NAs
#' @param y vector or matrix containing dependent variable in the model. \cr
#' The data must not contain NAs
#' @param G polynomial degree. \cr
#'     0: linear regression without quadratic and interactrion terms. \cr
#'     1: linear regression with interaction terms. \cr
#'     2: original Ivakhnenko quadratic polynomial.
#' @param criteria GMDH external criteria. Values: \cr
#'      \itemize{
#'       \item PRESS: Predicted Residual Error Sum of Squares. It take into account all information in data sample and it is computed without recalculating of system for each test point.\cr
#'       \item test: use x.test and y.test to estimate RMSE (Root Mean Squeare Errors). \cr
#'       \item ICOMP: Index of Informational Complexity. Like PRESS, it is computed without recalculating of system.
#'      }
#' @param x.test matrix with a sample randomly drawn from the initial data. This sample should not be included in X. \cr
#'    It is used when criteria = test. \cr
#' @param y.test vector or matrix with y values correspond with x.test values.
#'
#' @return An object of class 'combi'. This is a list with two elements: results and G. \cr
#'     Results is a list with two elements: \cr
#'     \itemize{
#'       \item coef: coeficients of final selected GMDH Combinatorial model.
#'       \item CV: external criteria value for selected model.
#'       }
#' @return G the grade of polynomial used in GMDH Combinatorial model.
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
#' mod <- gmdh.combi(X = x, y = y, criteria = "PRESS")
#' pred <- predict(mod, x.test)
#' summary(sqrt((pred - y.test)^2))
#'
#' @references
#' Bozdogan, H. and Haughton, D.M.A. (1998): "Information complexity criteria for regression models", Computational Statistics & Data Analysis, 28, pp. 51-76 <doi: 10.1016/S0167-9473(98)00025-5> \cr
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
gmdh.combi <- function(X, y, G = 2, criteria = c("PRESS", "test", "ICOMP"), x.test = NULL, y.test = NULL) {

  if (is.matrix(X) != TRUE)
    stop("X must be a matrix")
  if(ncol(X) < 2)
    stop("GMDH Combinatorial needs more than 1 regressor")
  if(any(is.na(X)) != FALSE)
    stop("X has NA values")
  if(any(is.na(y)) != FALSE)
    stop("y has NA values")
  if((ncol(X) > 4) & (G > 0))
    message("More than 4 regressors and G > 0 could be very computational expensive")

  switch(criteria,
         PRESS = return(gmdh.combi_1(X = X, y = y, G = G)),
         test = return(gmdh.combi_2(X= X, y = y, G = G, x.test = x.test, y.test = y.test)),
         ICOMP = return(gmdh.combi_3(X= X, y = y, G = G))
  )
}
