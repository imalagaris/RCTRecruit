# LoadData ---------------------------------------------------------------------
#' Load recruitment data. 
#' 
#' This function must be called before any other function in this package. LoadData<br>
#' checks the input data and stores the results internally for the session.<br>
#' Calling this function more than once in the same session will overwrite the<br>
#' previously created internal data.
#' @param data 
#'  Main dataset containing at least two columns:
#' * A `date` column with the calendar date of the screening
#' * A `enrolled` column with the number of subjects enrolled  
#' 
#' If the entries cover a period longer than 1 year, only the entries within <br>
#' one year prior to the latest date will be retained.<br>
#' <br>
#' @param date 
#' The name (symbol or string) of the column in the dataset with the calendar<br>
#' dates of active screening. All active calendar dates should be included, even<br>
#' if the recruitment for that date is 0. Only dates with truly non-active<br>
#' recruitment should be omitted. The date column must be:  
#' * an object inheriting from class the `Date` class
#' * or a character vector with a valid date format. 
#' @param enrolled 
#' The name (symbol or string) of the column in the dataset with the number of<br>
#' subjects recruited on the corresponding calendar date. It must be a numeric vector.
#' @return 
#' This function does not return any value. It runs several tests and proccesses<br>
#' the data and stores internally the results. It prints a message to the console<br>
#' if the data is successfully loaded  or an error message if there is an issue<br>
#' with the input data. Once the dataset is loaded, the following functions can<br>
#' be used:
#' * [Time2Nsubjects()]: simulates the number of weeks needed to recruit a<br>
#'  given number of subjects
#'  * [GetDistance()]: calculates the Euclidean distance between the<br>
#'  predicted and actual recruitment
#'  * [GetWeekPredCI()]: calculates the median recruitment with 95% CI for<br>
#'  up to the next 104 weeks (two years)
#'  
#' @examples 
#' # Load using names as symbols
#' LoadData(gripsYR1, ScreenDt, Enrolled)
#'
#' # Load using names as strings
#' LoadData(gripsYR1, "ScreenDt", "Enrolled")
#' 
#' # Load using base pipe operator
#' gripsYR1 |> LoadData(ScreenDt, Enrolled)
#' 
#' # Load using magrittr pipe operator
#' if (base::requireNamespace("magrittr", quietly = TRUE)) {
#'   library(magrittr)
#'   gripsYR1 %>% LoadData(ScreenDt, Enrolled) 
#' }
#' @family Links
#' @export
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

# Time2Nsubjects ---------------------------------------------------------------
#' Simulate the number of weeks needed to recruit a given number of subjects
#' @param nSub Number of subjects to recruit (default = 50L)
#' @param nSim 
#' Number of simulations to run (default = 1e4L). Accepted values are in the <br>
#' range of 1 to 10,000.
#' @param fillGaps Whether to fill recruitment gaps in the data (default = FALSE).<br>
#' Recruitment gaps are defined as any full week (Monday through Sunday) with<br>
#' no dates recorded in the loaded data. If at least one date is present within<br>
#' a given week, that week will not be considered a gap in recruitment.
#' @param cauchyWt Whether to use Cauchy weights for sampling. 
#'     If FALSE (default),<br>
#'     binomial weights are used.
#' @param efficiencyFactor 
#' An efficiency coefficient to apply to the recruitment rate (default = 1).<br>
#' If the efficiency of the recruitment process is expected to match<br>
#' the provided data, this value should be set to 1. If the recruitment<br>
#' process is expected to be slower, this value should less than 1. Finally,<br>
#' if the recruitment process is expected to proceed faster, this value should be<br>
#' greater than 1. Accepted values range from 0.1 to 2:<br>
#' * 0.1: Indicates that the recruitment rate is expected to be 10% of the original rate.
#' * 2.0: Indicates that the recruitment rate is expected to be double the original rate.
#' 
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
Time2Nsubjects <- \(
  nSub = 50L,
  nSim = 1e4L,
  fillGaps = FALSE,
  cauchyWt = FALSE,
  efficiencyFactor = 1
) {
  checkExportedFunctionsArgs()
  useFilled(fillGaps)
  the$useCauchy(cauchyWt)
  applyCoeff(efficiencyFactor)
  out <- c(the$weeks2Nsubjects(nSim, nSub), getCall())
  structure(out, class = "RCTNWeeks")
}

#' @export
print.RCTNWeeks <- function(x, ...) {
  log(msg$enrollWeeks, bold(x$cargs$nSub, 28), bold(x$CI[[2L]], 28))
  print(round(x$CI))
  invisible(x)
}

# GetDistance ------------------------------------------------------------------
#' Euclidean distance between predicted and actual recruitment
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
#' (res <- GetDistance(gripsYR2Weekly$enrolled))
#' str(res)
#' @family Links
#' @export
GetDistance <- \(
  target,
  nSim = 1e4L,
  fillGaps = FALSE,
  cauchyWt = FALSE,
  efficiencyFactor = 1
) {
  checkExportedFunctionsArgs()
  useFilled(fillGaps)
  the$useCauchy(cauchyWt)
  applyCoeff(efficiencyFactor)
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

#' @export
print.RCTDist <- function(x, ...) {
  print(round(x$CI))
  invisible(x)
}

# GetWeekPredCI ----------------------------------------------------------------
#' Calculate median recruitment with 95% CI for the next 104 weeks (two years)
#' @inheritParams Time2Nsubjects
#' @return
#' An object of `RCTPredCI` class with 5 elements.
#' 1. `predCI`: An 104x3 matrix with the 2.5%, 50% and 97.5% weekly percentiles
#' 1. `plot(yMax = NULL, Title = NULL)`:<br>
#'     Function which plots the results. It accepts the following arguments:
#'    - `yMax` sets the upper limit of the y-axis
#'    - `Title` sets the main title for the plot
#' 1. `pargs`:<br> 
#'     An environment which contains objects and functions used to construct<br>
#'     the plot. Additional plot configuration to what the `plot()` function<br>
#'     currently supports, can be achieved by modifying those objects
#' 1. `r e$commonOutput`
#' @examples
#' LoadData(gripsYR1, ScreenDt, Enrolled)
#' (res <- GetWeekPredCI(fillGaps = TRUE, efficiencyFactor = 1.5))
#' scenarios <- list(
#'   sc1 = GetWeekPredCI(),
#'   sc2 = GetWeekPredCI(cauchyWt = TRUE),
#'   sc3 = GetWeekPredCI(fillGaps = TRUE),
#'   sc4 = GetWeekPredCI(fillGaps = TRUE, efficiencyFactor = 1.2)
#' )
#' maxY <- sapply(scenarios, \(x) x$pargs$maxY) |> max()
#' defaultGraphicParams <- par(no.readonly = TRUE)
#' graphics::par(mfrow = c(2, 2), oma = c(0, 1, 0, 7), mar = c(4, 4, 3, 1))
#' for (x in scenarios) x$plot(yMax = maxY, Title = x$call.)
#' do.call(par, defaultGraphicParams)
#' @family Links
#' @export
GetWeekPredCI <- \(
  nSim = 1e4L,
  fillGaps = FALSE,
  cauchyWt = FALSE,
  efficiencyFactor = 1
) {
  checkExportedFunctionsArgs()
  useFilled(fillGaps)
  the$useCauchy(cauchyWt)
  applyCoeff(efficiencyFactor)
  out <- the$PredCIbyWk(nSim) |> rbind(rep(0, 3), ... = _) |> round()
  rownames(out) <- 0:(nrow(out) - 1)
  obj <- CreatePredCIplotObj(out)
  out <- c(getCall(), list(predCI = out, plot = obj$.f$predPlot, pargs = obj))
  structure(out, class = "RCTPredCI")
}

#' @export
print.RCTPredCI <- function(x, ...) {
  print(x$predCI |> utils::head())
  cat("\t", "...", "\n")
  print(x$predCI |> utils::tail())
  invisible(x)
}
