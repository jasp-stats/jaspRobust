context("Robust ANCOVA")

# Reference tables captured via jaspTools::makeTestTable on debug.csv with
# set.seed(1).

.initAncovaRobustOptions <- function() {
  options <- list()
  options$dependent       <- ""
  options$fixedFactors    <- ""
  options$covariates      <- ""
  options$robustMethod    <- "trimmedMeans"
  options$trimProportion  <- 0.2
  options$bootstrapSamples <- 599
  options$descriptivesTable <- FALSE
  options$descriptivePlotHorizontalAxis <- ""
  options$descriptivePlotSeparateLines  <- ""
  options$descriptivePlotSeparatePlot   <- ""
  options$descriptivePlotErrorBar       <- FALSE
  options$descriptivePlotErrorBarType   <- "ci"
  options$descriptivePlotCiLevel        <- 0.95
  options$rainCloudHorizontalAxis    <- ""
  options$rainCloudSeparatePlots     <- ""
  options$rainCloudHorizontalDisplay <- FALSE
  options$rainCloudYAxisLabel        <- ""
  return(options)
}

# ---- Robust ANCOVA, trimmed means -----------------------------------------

test_that("Robust ANCOVA (trimmed means) main table matches", {
  options <- .initAncovaRobustOptions()
  options$dependent      <- "contNormal"
  options$fixedFactors   <- "contBinom"
  options$covariates     <- "contGamma"
  options$robustMethod   <- "trimmedMeans"
  options$trimProportion <- 0.2
  options$descriptivesTable <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("AncovaRobust", "debug.csv", options)

  table <- results[["results"]][["ancovaRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(-0.429027548643857, 1.19160659701138, 0.197333889, 28, 19, 0.207377004136249,
                                             0.295323904506083, 1.29108926966631, 0.381289524183761, -0.334023763648355,
                                             1.07654360943883, 0.678039819, 35, 25, 0.160881045013004, 0.258158427865385,
                                             1.43810886193043, 0.371259922895238, -0.313353019183238, 0.88873727563623,
                                             1.281209304, 42, 30, 0.205171788939185, 0.223339563750743, 1.28813777279324,
                                             0.287692128226496, -0.591772256204538, 0.69583135571223, 1.800714959,
                                             42, 32, 0.828932797134438, 0.239210500177242, 0.217505292264742,
                                             0.0520295497538462, -1.04568493748888, 0.754565858188882, 3.491491238,
                                             20, 14, 0.650968217010081, 0.316483173488053, 0.459928210545117,
                                             -0.14555953965))

  table <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(0, 0.8649064354161, -0.18943073975, -0.2223981035, 58, "contNormal",
                                             0.568333486814063, 0, 1.4728222018503, 1.79220918169444, 1.6961274775,
                                             58, "contGamma", 1.22469792658697, 1, 0.7927275718572, -0.332608387307692,
                                             -0.405769511, 42, "contNormal", 0.58496035543894, 1, 1.2583327140888,
                                             1.72278395003846, 1.641466777, 42, "contGamma", 0.794984474988436))
})

# ---- Robust ANCOVA, bootstrap --------------------------------------------

test_that("Robust ANCOVA (bootstrap) main table matches", {
  options <- .initAncovaRobustOptions()
  options$dependent       <- "contNormal"
  options$fixedFactors    <- "contBinom"
  options$covariates      <- "contGamma"
  options$robustMethod    <- "trimmedMeansBootstrap"
  options$trimProportion  <- 0.2
  options$bootstrapSamples <- 199

  set.seed(1)
  results <- jaspTools::runAnalysis("AncovaRobust", "debug.csv", options)

  table <- results[["results"]][["ancovaRobustTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list(-0.521452546614296, 1.28403159498182, 0.197333889, 28, 19, 0.190954773869347,
                                             1.29108926966631, 0.381289524183761, -0.41787522738979, 1.16039507318027,
                                             0.678039819, 35, 25, 0.120603015075377, 1.43810886193043, 0.371259922895238,
                                             -0.395009194609794, 0.970393451062785, 1.281209304, 42, 30,
                                             0.165829145728643, 1.28813777279324, 0.287692128226496, -0.679185834411559,
                                             0.783244933919251, 1.800714959, 42, 32, 0.829145728643216, 0.217505292264742,
                                             0.0520295497538462, -1.11298097438002, 0.82186189508002, 3.491491238,
                                             20, 14, 0.678391959798995, -0.459928210545117, -0.14555953965
  ))
})

# ---- Robust ANCOVA descriptives plot --------------------------------------

test_that("Robust ANCOVA descriptives plot matches", {
  options <- .initAncovaRobustOptions()
  options$dependent    <- "contNormal"
  options$fixedFactors <- "contBinom"
  options$covariates   <- "contGamma"
  options$descriptivePlotHorizontalAxis <- "contGamma"
  options$descriptivePlotErrorBar       <- TRUE
  options$descriptivePlotErrorBarType   <- "ci"
  options$descriptivePlotCiLevel        <- 0.95

  set.seed(1)
  results <- jaspTools::runAnalysis("AncovaRobust", "debug.csv", options)

  plotName <- results[["results"]][["descriptivesPlotContainer"]][["collection"]][[1]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "ancovarobust-descriptives")
})

# ---- Robust ANCOVA descriptives with missing values -----------------------

test_that("Robust ANCOVA descriptives use only the rows the model uses", {
  # rows 3 (group A) and 6 (group B) have no x and row 10 (group B) has no y;
  # WRS2::ancova drops all three, so the descriptives must use 15 complete rows per group.
  # Expected values computed in base R on those rows: trimmed mean and Winsorized SD with
  # floor(0.2 * 15) = 3 values trimmed or Winsorized at each end, MAD = 1.4826 * median |x - median|.
  dataset <- data.frame(
    y = c(6.4, 8.1, 14, 7.9, 8.6, 15, 8.4, 7.4, 3.9, NA, 7.8, 6.3, 6.5, 5.8, 8.6, 6, 6,
          7.1, 7.2, 7.8, 9.5, 7.9, 8.3, 7.4, 6.5, 5.9, 9.2, 6.2, 8.1, 6.4, 9, 7.7, 7.5),
    x = c(8.9, 11.1, NA, 10.8, 8.8, NA, 11.7, 10.5, 10.9, 9.5, 9.1, 8.3, 10, 7.4, 11.2, 8.5, 7.1,
          10.7, 9.1, 9.3, 13.1, 11.2, 11, 9.9, 8.8, 6.6, 9.5, 8.7, 9.8, 9.8, 10.2, 9.8, 8.1),
    g = factor(c("A", "B", "A", "A", "B", "B", "A", "B", "A", "B", "B", "A", "B", "A", "B", "A", "B",
                 "A", "B", "A", "B", "A", "B", "A", "B", "A", "B", "A", "B", "A", "B", "A", "B"))
  )
  options <- .initAncovaRobustOptions()
  options$dependent      <- "y"
  options$fixedFactors   <- "g"
  options$covariates     <- "x"
  options$robustMethod   <- "trimmedMeans"
  options$trimProportion <- 0.2
  options$descriptivesTable <- TRUE

  set.seed(1)
  results <- jaspTools::runAnalysis("AncovaRobust", dataset, options)

  table <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("A", 1.03782, 6.81111111111111, 6.4, 15, "y", 0.789092305542683,
         "A", 1.63086, 9.6, 9.8, 15, "x", 0.977021713459547,
         "B", 1.03782, 7.95555555555556, 8.1, 15, "y", 0.6007931265855,
         "B", 1.4826, 9.77777777777778, 9.8, 15, "x", 0.906694677438463))
})
