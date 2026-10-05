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

#' Robust ANOVA
#'
#' Robust ANOVA analyzes differences between group means using robust methods from the WRS2 package. These methods do not assume normality or homogeneity of variances.
#' ## Assumptions
#' - The independent variables are categorical, the dependent variable is continuous.
#' - The groups are independent.
#'
#' @param bootstrapSamples, The number of bootstrap resamples to use. Default is 599.
#' @param dependent, The continuous outcome variable.
#' @param descriptivePlotErrorBar, Display error bars in the plot. The error bars can represent either confidence intervals or standard errors.
#'    Defaults to \code{TRUE}.
#' @param descriptivePlotHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param descriptivePlotSeparateLines, By placing an independent variable in this box, different lines corresponding to the different levels of the independent variable will be displayed.
#' @param descriptivePlotSeparatePlot, By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param descriptivesTable, Per-cell sample size, trimmed (or untrimmed) mean, median, Winsorized standard deviation, and median absolute deviation.
#'    Defaults to \code{FALSE}.
#' @param effectSizeTable, Algina-Keselman-Penfield robust standardised mean differences (ξ) for each pair of factor levels. Computed via WRS2::akp.effect on a one-way design.
#'    Defaults to \code{FALSE}.
#' @param fixedFactors, The categorical grouping variables. Trimmed means support up to three factors; medians support up to two; the bootstrap method supports a single factor (one-way) only.
#' @param postHocCi, When this option is selected, the confidence interval for the trimmed mean difference is displayed.
#'    Defaults to \code{FALSE}.
#' @param postHocCorrectionBonferroni, This correction is considered conservative. The risk of Type I error is reduced, however the statistical power decreases as well.
#'    Defaults to \code{FALSE}.
#' @param postHocCorrectionHochberg, Hochberg's (1988) sharper Bonferroni procedure. This is the default correction used by WRS2's lincon function.
#'    Defaults to \code{TRUE}.
#' @param postHocCorrectionHolm, Also called sequential Bonferroni, and considered less conservative than the Bonferroni method.
#'    Defaults to \code{FALSE}.
#' @param postHocSignificanceFlag, Add asterisks to the table to indicate 3 levels of significance.
#'    Defaults to \code{FALSE}.
#' @param rainCloudHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param rainCloudHorizontalDisplay, Changes the orientation of the raincloud difference plot so that the x-axis represents the dependent variable.
#'    Defaults to \code{FALSE}.
#' @param rainCloudSeparatePlots, By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param robustMethod, Select the robust estimation method to use for the analysis.
#' @param trimProportion, The proportion of observations to trim from each tail. Default is 0.2 (20%% trimming).
AnovaRobust <- function(
          data = NULL,
          version = "1",
          bootstrapSamples = 599,
          dependent = list(types = list(), value = ""),
          descriptivePlotCiLevel = 0.95,
          descriptivePlotErrorBar = TRUE,
          descriptivePlotErrorBarType = "ci",
          descriptivePlotHorizontalAxis = list(types = list(), value = ""),
          descriptivePlotSeparateLines = list(types = list(), value = ""),
          descriptivePlotSeparatePlot = list(types = list(), value = ""),
          descriptivesTable = FALSE,
          effectSizeTable = FALSE,
          fixedFactors = list(types = list(), value = list()),
          normalizeErrorBarsDescriptives = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          postHocCi = FALSE,
          postHocCiLevel = 0.95,
          postHocCorrectionBonferroni = FALSE,
          postHocCorrectionHochberg = TRUE,
          postHocCorrectionHolm = FALSE,
          postHocSignificanceFlag = FALSE,
          postHocTerms = list(types = list(), value = list()),
          rainCloudHorizontalAxis = list(types = list(), value = ""),
          rainCloudHorizontalDisplay = FALSE,
          rainCloudSeparatePlots = list(types = list(), value = ""),
          rainCloudYAxisLabel = "",
          robustMethod = "trimmedMeans",
          trimProportion = 0.2) {

   defaultArgCalls <- formals(jaspRobust::AnovaRobust)
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

   optionsWithFormula <- c("dependent", "descriptivePlotHorizontalAxis", "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot", "fixedFactors", "postHocTerms", "rainCloudHorizontalAxis", "rainCloudSeparatePlots", "robustMethod")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRobust", "AnovaRobust", "AnovaRobust.qml", options, version, TRUE))
}