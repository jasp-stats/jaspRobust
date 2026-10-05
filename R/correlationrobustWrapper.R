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

#' Robust Correlation
#'
#' Robust correlation computes percentage bend or Winsorized correlations (WRS2 package). These methods are resistant to outliers that can distort Pearson's r.
#' ## Assumptions
#' - Continuous variables.
#' - Independence of observations.
#' - Unlike Pearson's r, robust correlations do not assume bivariate normality.
#'
#' @param alternative, Specify the directional hypothesis used to compute the p-value of each correlation.
#' \itemize{
#'   \item \code{"twoSided"}: Test whether the correlation differs from zero in either direction.
#'   \item \code{"greater"}: Test whether the correlation is greater than zero.
#'   \item \code{"less"}: Test whether the correlation is less than zero.
#' }
#' @param correlationMethod, Percentage bend correlation downweights values in the tails. Winsorized correlation replaces extreme values with less extreme ones before computing the correlation.
#' @param descriptivesTable, Per-variable sample size, trimmed mean, median, Winsorized standard deviation, and median absolute deviation.
#'    Defaults to \code{FALSE}.
#' @param sampleSize, Display the number of complete pairwise observations used in each correlation.
#'    Defaults to \code{FALSE}.
#' @param scatterPlot, Displays scatter plots with marginal density plots and a linear regression line for each pair of variables.
#'    Defaults to \code{FALSE}.
#' @param significanceFlagged, Append asterisks to correlations whose p-value is below 0.05 (*), 0.01 (**), or 0.001 (***).
#'    Defaults to \code{FALSE}.
#' @param trimProportion, Bending constant β for percentage bend correlation, or Winsorization proportion for the Winsorized correlation. Also used for the descriptives table. Default is 0.2.
#' @param variables, Two or more continuous variables. All pairwise robust correlations are reported.
CorrelationRobust <- function(
          data = NULL,
          version = "1",
          alternative = "twoSided",
          correlationMethod = "percentageBend",
          descriptivesTable = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          sampleSize = FALSE,
          scatterPlot = FALSE,
          significanceFlagged = FALSE,
          trimProportion = 0.2,
          variables = list(types = list(), value = list())) {

   defaultArgCalls <- formals(jaspRobust::CorrelationRobust)
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

   optionsWithFormula <- c("correlationMethod", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRobust", "CorrelationRobust", "CorrelationRobust.qml", options, version, TRUE))
}