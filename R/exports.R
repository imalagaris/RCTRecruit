#' Load recruitment data
#' @param data Main dataset
#' @param date Date column
#' @param enrolled Enrolled column
#' @return NULL
#' @examples LoadData(gripsYR1, ScreenDt, Enrolled)
#' LoadData(gripsYR1, "ScreenDt", "Enrolled")
#' gripsYR1 |> LoadData(ScreenDt, Enrolled)
#' @family Links
#' @export
#' @rawRd % f1
LoadData <- \(data, date, enrolled) {
  if (is.null(data)) stop("data is NULL")
  if (!is.data.frame(data)) stop("data must be a dataframe")
  cargs <- getCall()$cargs
  the$raw <- checkArgs()
  the$datWeeks <- days2weeks(the$raw$date, the$raw$enrolled)
  the$train <- the$datWeeks$enrolled
  the$TrainVector <- the$train
  the$Trainfilled <- fillEmptyWeeks()
  the$cppModule <- methods::new(rct, the)
  exportModuleMethods(the$cppModule)
  LoadSuccess(cargs)
}

#' Simulate number of weeks needed to recruit a given number of subjects
#' @param nSub Number of subjects to recruit (default = 50L)
#' @param nSim Number of simulations to run (default = 1e4L)
#' @param fillGaps Whether to fill gaps in the data (default = FALSE)
#' @param cauchyWt Whether to use Cauchy weights for sampling. 
#'     If FALSE (default),<br>
#'     binomial weights are used.
#' @param coeff A coefficient to apply to the recruitment rate (default = 1)
#' @return
#' An object of `RCTNWeeks` class with four elements.
#' 1. `weeks` is an integer vector with length equal to `nSim` containing the
#'     simulation results.
#' 1. `CI` shows the median and the 95% CI.
#' 1. `r e$commonOutput`
#' @examples
#' LoadData(gripsYR1, ScreenDt, Enrolled)
#' (res <- Time2Nsubjects())
#' str(res)
#' @family Links
#' @export
#' @rawRd % f2
Time2Nsubjects <- \(
  nSub = 50L,
  nSim = 1e4L,
  fillGaps = FALSE,
  cauchyWt = FALSE,
  coeff = 1
) {
  checkExportedFunctionsArgs()
  useFilled(fillGaps)
  the$useCauchy(cauchyWt)
  applyCoeff(coeff)
  out <- c(the$weeks2Nsubjects(nSim, nSub), getCall())
  structure(out, class = "RCTNWeeks")
}

#' Euclidean distance between prediction and actual recruitment
#' @param target A vector with the actual recruitment by week
#' @inheritParams Time2Nsubjects
#' @return
#' An object of `RCTDist` class with four elements.
#' 1. `dist`: 
#'     A numeric vector with length equal to `nSim` containing the simulated<br>
#'     Euclidean distance.
#' 1. `CI`: A numeric vector with the median and the 95% CI Euclidean distance.
#' 1. `r e$commonOutput`
#' @examples
#' LoadData(gripsYR1, ScreenDt, Enrolled)
#' (res <- GetDistance(gripsWeeklyYR2$enrolled))
#' str(res)
#' @family Links
#' @export
#' @rawRd % f3
GetDistance <- \(
  target,
  nSim = 1e4L,
  fillGaps = FALSE,
  cauchyWt = FALSE,
  coeff = 1
) {
  checkExportedFunctionsArgs()
  useFilled(fillGaps)
  the$useCauchy(cauchyWt)
  applyCoeff(coeff)
  target <- fixEnrolled(target)
  len <- length(the$train)
  if (length(target) < len) stop("target is smaller")
  if (length(target) > len) target <- target[seq.int(len)]
  the$setTarget(target)
  dist <- the$getDistance(nSim)
  CI <- stats::quantile(x = dist, probs = c(.025, .5, .975))
  out <- c(list(dist = dist, CI = CI), getCall())
  structure(out, class = "RCTDist")
}

# GetWeekPredCI ----------------------------------------------------------
#' Calculate median recruitment with 95% CI for the next 104 weeks (two years)
#' @inheritParams Time2Nsubjects
#' @return
#' An object of `RCTPredCI` class with 5 elements.
#' 1. `predCI`: An 104x3 matrix with the 2.5%, 50% and 97.5% weekly percentiles
#' 1. `plot(yMax = NULL, Title = NULL)`:<br>
#'     Function which plots the results. It accepts the following arguments:
#'    - `yMax` sets the high limit of the y-axis
#'    - `Title` sets the main title for the plot
#' 1. `pargs`:<br> 
#'     An environment which contains objects and functions used to construct<br>
#'     the plot. Additional plot configuration to what the `plot()` function<br>
#'     currently supports, can be achieved by modifying those objects
#' 1. `r e$commonOutput`
#' @examples
#' LoadData(gripsYR1, ScreenDt, Enrolled)
#' (res <- GetWeekPredCI(fillGaps = TRUE, coeff = 1.5))
#' scenarios <- list(
#'   sc1 = GetWeekPredCI(),
#'   sc2 = GetWeekPredCI(cauchyWt = TRUE),
#'   sc3 = GetWeekPredCI(fillGaps = TRUE),
#'   sc4 = GetWeekPredCI(fillGaps = TRUE, coeff = 1.2)
#' )
#' maxY <- sapply(scenarios, \(x) x$pargs$maxY) |> max()
#'
#' graphics::par(mfrow = c(2, 2), oma = c(0, 1, 0, 7), mar = c(4, 4, 3, 1))
#' for (x in scenarios) x$plot(yMax = maxY, Title = x$call.)
#' @family Links
#' @export
#' @rawRd % f4
GetWeekPredCI <- \(
  nSim = 1e4L,
  fillGaps = FALSE,
  cauchyWt = FALSE,
  coeff = 1
) {
  checkExportedFunctionsArgs()
  useFilled(fillGaps)
  the$useCauchy(cauchyWt)
  applyCoeff(coeff)
  out <- the$PredCIbyWk(nSim) |> rbind(rep(0, 3), ... = _) |> round()
  rownames(out) <- 0:(nrow(out) - 1)
  obj <- CreatePredCIplotObj(out)
  out <- c(getCall(), list(predCI = out, plot = obj$predPlot, pargs = obj))
  structure(out, class = "RCTPredCI")
}
