#' Load recruitment data
#' @param data Main dataset
#' @param date Date column
#' @param enrolled Enrolled column
#' @return NULL
#' @examples LoadData(gripsYR1, ScreenDt, Enrolled)
#' LoadData(gripsYR1, "ScreenDt", "Enrolled")
#' gripsYR1 |> LoadData(ScreenDt, Enrolled)
#' @family RCTRecruit functions
#' @export
#' @rawRd % f1
LoadData <- \(data, date, enrolled) {
  if (is.null(data)) stop("data is NULL")
  if (!is.data.frame(data)) stop("data must be a dataframe")
  cargs <- getCall()
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
#' @param nSub Number of subjects to recruit (default = 50)
#' @param nSim Number of simulations to run (default = 1e4)
#' @param fillGaps Whether to fill gaps in the data (default = FALSE)
#' @param cauchyWt Whether to use Cauchy weights for sampling (default = FALSE).
#'    If FALSE, binomial weights are used.
#' @param coeff A coefficient to apply to the recruitment rate (default = 1)
#' @return A `list` with two elements. The first element `weeks` is an integer
#'     vector with length equal to `nSim` containing the results of the
#'     simulation. The second `CI` shows the median and the 95% CI.
#' @examples
#' LoadData(gripsYR1, ScreenDt, Enrolled)
#' (res <- Time2Nsubjects())
#' str(res)
#' @family RCTRecruit functions
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
  out <- c(cargs = list(getCall()), the$weeks2Nsubjects(nSim, nSub))
  structure(out, class = "RCTNWeeks")
}

#' Euclidean distance between prediction and actual recruitment
#' @param target A vector with the actual recruitment by week
#' @inheritParams Time2Nsubjects
#' @return A list with two elements. The first element `dist` is a numeric
#'     vector with length equal to `nSim` containing the simulated Euclidean
#'     distance. The second `CI` shows the median and the 95%CI Euclidean
#'     distance.
#' @examples
#' LoadData(gripsYR1, ScreenDt, Enrolled)
#' (res <- GetDistance(gripsWeeklyYR2$enrolled))
#' str(res)
#' @family RCTRecruit functions
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
  out <- list(dist = dist, CI = CI, cargs = getCall())
  structure(out, class = "RCTDist")
}

# GetWeekPredCI ----------------------------------------------------------
#' Calculate median recruitment with 95% CI for the next 104 weeks
#' @inheritParams Time2Nsubjects
#' @return An 104x3 matrix with the 2.5%, 50% and 97.5% weekly percentiles
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
#' @family RCTRecruit functions
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
  out <- list(predCI = out, plot = obj$predPlot, pargs = obj)
  out[["cargs"]] <- getCall()
  out[["call."]] <- deparse(sys.call())
  structure(out, class = "RCTPredCI")
}
