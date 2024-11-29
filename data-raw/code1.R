LoadData(gripsYR1, ScreenDt, Enrolled)
res <- GetWeekPredCI()
res$predPlot()
res$plot_$lines_

scenarios <- list(
  sc1 = GetWeekPredCI(),
  sc2 = GetWeekPredCI(fillGaps = TRUE),
  sc3 = GetWeekPredCI(fillGaps = TRUE, coeff = 1.2)
)

maxY <- sapply(scenarios, \(x) x$plot_$maxY) |> max()

graphics::par(mfrow = c(1, 3))
for (x in scenarios) x$predPlot(yMax = maxY, Title = x$call.)


.l <- \(x, el) {
    a <- sys.call(0)
    return(a)
    str <- deparse(substitute(el))
    xstr <- if (substr(str, 0, 1) == "[") "x" else "x$"
    paste0("\\(x) ", xstr, str) |> str2lang() |> eval() |> lapply(x, FUN = _)
}

aek <- function(x, y, z) {
  sys.call()
}

scenarios$sc3$plot_$initArgs |>
  (\(x) names(x) |> lapply(\(y) paste0(y, " = ", x[[y]])) )() |>
  unlist() |>
  paste0(collapse = ", ") |>
  paste0("(", ... = _, ")")




LoadData(gripsYR1, ScreenDt, Enrolled)
(res <- GetWeekPredCI(fillGaps = TRUE, coeff = 1.5))
scenarios <- list(
  sc1 = GetWeekPredCI(),
  sc2 = GetWeekPredCI(cauchyWt = TRUE),
  sc3 = GetWeekPredCI(fillGaps = TRUE),
  sc4 = GetWeekPredCI(fillGaps = TRUE, coeff = 1.2)
)
maxY <- sapply(scenarios, \(x) x$plot_$maxY) |> max()

graphics::par(mfrow = c(2, 2))
for (x in scenarios) x$predPlot(yMax = maxY, argsAsTitle = T)

a <- function(x =5, y = 6) {
  sys.call()
}
