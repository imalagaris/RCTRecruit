# LoadData ---------------------------------------------------------------
#' Load recruitment data
#' @param data Main dataset
#' @param date Date column
#' @param enrolled Enrolled column
#' @return NULL
#' @export
#' @examples LoadData(gripsYR1, ScreenDt, Enrolled)
LoadData <- \(data, date, enrolled) {
  if (is.null(data)) stop("data is NULL")
  if (!("data.frame" %in% class(data))) stop("data must be a dataframe")
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

# Time2Nsubjects ----------------------------------------------------------
#' Simulate number of weeks needed to recruit a given number of subjects
#' @param nSub Number of subjects to recruit (default = 50)
#' @param nSim Number of simulations to run (default = 1e4)
#' @param fillGaps Whether to fill gaps in the data (default = FALSE)
#' @param cauchyWt Whether to use Cauchy weights for sampling (default = FALSE).
#'    If FALSE, binomial weights are used.
#' @param coeff A coefficient to apply to the recruitment rate (default = 1)
#' @return A list with two elements. The first element `weeks` is an integer
#'     vector with length equal to `nSim` containing the results of the
#'     simulation. The second `CI` shows the median and the 95%CI.
#' @export
#' @examples
#' LoadData(gripsYR1, ScreenDt, Enrolled)
#' res <- Time2Nsubjects()
#' str(res)
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
  out <- the$weeks2Nsubjects(nSim, nSub)
  log(msg$enrollWeeks, bold(nSub, 28), bold(out$CI[[2L]], 28))
  print(round(out$CI))
  invisible(out)
}

# GetDistance ------------------------------------------------------------
#' Calculate CI of Euclidean distance of predicted recruitment with actual
#'     recruitment
#' @param target A vector with the actual recruitment by week
#' @inheritParams Time2Nsubjects
#' @return A list with two elements. The first element `dist` is a numeric
#'     vector with length equal to `nSim` containing the simulated Euclidean
#'     distance. The second `CI` shows the median and the 95%CI Euclidean
#'     distance.
#' @export
#' @examples
#' LoadData(gripsYR1, ScreenDt, Enrolled)
#' res <- GetDistance(gripsWeeklyYR2$enrolled)
#' str(res)
GetDistance <- \(
  target,
  nSim = 1e4L,
  fillGaps = FALSE,
  cauchyWt = FALSE,
  coeff = 1
) {
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
  print(round(CI))
  invisible(list(dist = dist, CI = CI))
}

# GetWeekPredCI ----------------------------------------------------------
#' Calculate median recruitment with CI for the next 52 weeks
#' @inheritParams Time2Nsubjects
#' @return An 52x3 matrix with the 2.5%, 50% and 97.5% percentiles for each week
#' @export
#' @examples
#' LoadData(gripsYR1, ScreenDt, Enrolled)
#' res <- GetWeekPredCI(fillGaps = TRUE, coeff = 1.5)
#' str(res)
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
  utils::head(out) |> logPrint()
  utils::tail(out) |> logPrint()
  invisible(out)
}
