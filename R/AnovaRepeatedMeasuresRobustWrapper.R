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

#' Robust Repeated Measures ANOVA
#'
#' Robust Repeated Measures ANOVA analyzes differences between dependent group means using robust methods from the WRS2 package. No assumptions of normality, homogeneity, or sphericity are required.
#' ## Assumptions
#' - The dependent variable is continuous.
#' - The observations within each subject are dependent (repeated measures).
#'
#' @param betweenSubjectFactors, Optional single between-subjects grouping variable for mixed designs.
#' @param bootstrapSamples, The number of bootstrap resamples to use. Default is 599.
#' @param descriptivePlotErrorBar, Display error bars in the plot. The error bars can represent either confidence intervals or standard errors.
#'    Defaults to \code{TRUE}.
#' @param descriptivePlotHorizontalAxis, Select the independent variable that should be displayed on the horizontal axis of the plot.
#' @param descriptivePlotSeparateLines, By placing an independent variable in this box, different lines corresponding to the different levels of the independent variable will be displayed.
#' @param descriptivePlotSeparatePlot, By placing an independent variable in this box, different plots corresponding to the different levels of the independent variable will be displayed.
#' @param descriptivesTable, Per-cell sample size, trimmed mean, median, Winsorized standard deviation, and median absolute deviation across the within-subjects (and optional between-subjects) factor levels.
#'    Defaults to \code{FALSE}.
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
#' @param repeatedMeasuresCells, The separate columns in the data frame that represent the levels of the repeated measure(s) factor(s).
#' @param repeatedMeasuresFactors, The within-subjects (repeated measures) variable. Define one factor and label its levels. Robust RM ANOVA currently supports a single within-subjects factor.
#' @param robustMethod, Select the robust estimation method to use for the analysis.
#' @param trimProportion, The proportion of observations to trim from each tail. Default is 0.2 (20%% trimming).
AnovaRepeatedMeasuresRobust <- function(
          data = NULL,
          version = "1",
          betweenSubjectFactors = list(types = list(), value = ""),
          bootstrapSamples = 599,
          descriptivePlotCiLevel = 0.95,
          descriptivePlotErrorBar = TRUE,
          descriptivePlotErrorBarType = "ci",
          descriptivePlotHorizontalAxis = list(types = list(), value = ""),
          descriptivePlotSeparateLines = list(types = list(), value = ""),
          descriptivePlotSeparatePlot = list(types = list(), value = ""),
          descriptivesTable = FALSE,
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
          repeatedMeasuresCells = list("", ""),
          repeatedMeasuresFactors = list(list(levels = list("Level 1", "Level 2"), name = "RM Factor 1")),
          robustMethod = "trimmedMeans",
          trimProportion = 0.2) {

   defaultArgCalls <- formals(jaspRobust::AnovaRepeatedMeasuresRobust)
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

   optionsWithFormula <- c("betweenSubjectFactors", "descriptivePlotHorizontalAxis", "descriptivePlotSeparateLines", "descriptivePlotSeparatePlot", "postHocTerms", "rainCloudHorizontalAxis", "rainCloudSeparatePlots", "repeatedMeasuresCells", "repeatedMeasuresFactors", "robustMethod")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspRobust", "AnovaRepeatedMeasuresRobust", "AnovaRepeatedMeasuresRobust.qml", options, version, TRUE))
}