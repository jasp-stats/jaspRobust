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

#' Robust ANCOVA
#'
#' Robust ANCOVA compares trimmed means of exactly two groups while adjusting for a covariate, using a nonparametric running interval smoother. No parametric assumptions about homogeneity of regression slopes or equal variances are required. Without a covariate, a robust one-way ANOVA is performed instead.
#' ## Assumptions
#' - The independent variable is categorical with exactly two levels.
#' - The dependent variable and covariate are continuous.
#' - The groups are independent.
#'
#' @param bootstrapSamples, The number of bootstrap resamples to use. Default is 599.
#' @param covariates, An optional continuous covariate to adjust for.
#' @param dependent, The continuous outcome variable.
#' @param descriptivePlotErrorBar, Display error bars in the plot. The error bars can represent either confidence intervals or standard errors.
#'    Defaults to \code{TRUE}.
#' @param descriptivePlotHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param descriptivePlotSeparateLines, By placing an independent variable in this box, different lines corresponding to the different levels of the independent variable will be displayed.
#' @param descriptivePlotSeparatePlot, By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param descriptivesTable, Per-group sample size, trimmed mean, median, Winsorized standard deviation, and median absolute deviation for the dependent variable and each covariate.
#'    Defaults to \code{FALSE}.
#' @param fixedFactors, A categorical grouping variable with exactly two levels.
#' @param rainCloudHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param rainCloudHorizontalDisplay, Changes the orientation of the raincloud difference plot so that the x-axis represents the dependent variable.
#'    Defaults to \code{FALSE}.
#' @param rainCloudSeparatePlots, By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param robustMethod, Select the robust estimation method to use for the analysis.
#' @param trimProportion, The proportion of observations to trim from each tail. Default is 0.2 (20%% trimming).
AncovaRobust <- function(
          data = NULL,
          version = "1",
          bootstrapSamples = 599,
          covariates = list(types = list(), value = ""),
          dependent = list(types = list(), value = ""),
          descriptivePlotCiLevel = 0.95,
          descriptivePlotErrorBar = TRUE,
          descriptivePlotErrorBarType = "ci",
          descriptivePlotHorizontalAxis = list(types = list(), value = ""),
          descriptivePlotSeparateLines = list(types = list(), value = ""),
          descriptivePlotSeparatePlot = list(types = list(), value = ""),
          descriptivesTable = FALSE,
          fixedFactors = list(types = list(), value = ""),
          normalizeErrorBarsDescriptives = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          rainCloudHorizontalAxis = list(types = list(), value = ""),
          rainCloudHorizontalDisplay = FALSE,
          rainCloudSeparatePlots = list(types = list(), value = ""),
          rainCloudYAxisLabel = "",
          robustMethod = "trimmedMeans",
          trimProportion = 0.2) {

   defaultArgCalls <- formals(jaspRobust::AncovaRobust)
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

   optionsWithFormula <- c("covariates", "dependent", "descriptivePlotHorizontalAxis", "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot", "fixedFactors", "rainCloudHorizontalAxis", "rainCloudSeparatePlots", "robustMethod")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRobust", "AncovaRobust", "AncovaRobust.qml", options, version, TRUE))
}