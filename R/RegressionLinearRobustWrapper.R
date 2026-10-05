#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' Robust Linear Regression
#'
#' Robust linear regression uses M-estimation (MASS::rlm) to fit a linear model that is resistant to outliers. Unlike OLS, it iteratively downweights observations with large residuals.
#' ## Assumptions
#' - Continuous response variable.
#' - Linearity: The response variable is linearly related to all predictors.
#' - Independence of observations.
#' - Unlike OLS, robust regression does not assume normality or homoscedasticity of residuals.
#'
#' @param coefficientCi, Display Wald-type confidence intervals for the regression coefficients at the chosen confidence level.
#'    Defaults to \code{FALSE}.
#' @param coefficientEstimate, Display the estimated regression coefficients with standard errors, Wald t-statistics, and p-values.
#'    Defaults to \code{TRUE}.
#' @param covariates, Continuous predictors entered as numeric covariates.
#' @param dependent, The continuous outcome variable to be regressed on the predictors.
#' @param descriptivesTable, Per-variable sample size, trimmed mean, median, Winsorized standard deviation, and median absolute deviation for the dependent variable and each covariate. The trimming proportion is used only for these descriptives; the robust regression fit itself uses the chosen estimation method.
#'    Defaults to \code{FALSE}.
#' @param descriptivesTrimProportion, Trimming proportion used for the trimmed mean and Winsorized SD in the descriptives table. Default is 0.2.
#' @param estimationMethod, Huber downweights outliers gradually. Bisquare (Tukey) can fully reject extreme outliers. MM-estimation combines high breakdown point with high efficiency.
#' @param factors, Categorical predictors. Each factor is dummy coded with the first level as the reference.
#' @param residualQqPlot, Normal quantile-quantile plot of the standardised residuals to assess the residual distribution.
#'    Defaults to \code{FALSE}.
#' @param residualVsFittedPlot, Plot residuals against fitted values with a loess smoother to inspect non-linearity or heteroscedasticity that survives the robust fit.
#'    Defaults to \code{FALSE}.
#' @param weightsPlot, Shows the weight assigned to each observation during estimation. Downweighted observations (weight < 1) indicate potential outliers.
#'    Defaults to \code{FALSE}.
RegressionLinearRobust <- function(
          data = NULL,
          version = "1",
          coefficientCi = FALSE,
          coefficientCiLevel = 0.95,
          coefficientEstimate = TRUE,
          covariates = list(types = list(), value = list()),
          dependent = list(types = list(), value = ""),
          descriptivesTable = FALSE,
          descriptivesTrimProportion = 0.2,
          estimationMethod = "huber",
          factors = list(types = list(), value = list()),
          plotHeight = 320,
          plotWidth = 480,
          residualQqPlot = FALSE,
          residualVsFittedPlot = FALSE,
          weightsPlot = FALSE) {

   defaultArgCalls <- formals(jaspRobust::RegressionLinearRobust)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("covariates", "dependent", "estimationMethod", "factors")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRobust", "RegressionLinearRobust", "RegressionLinearRobust.qml", options, version, TRUE))
}