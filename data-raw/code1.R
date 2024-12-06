LoadData(gripsYR1, ScreenDt, Enrolled)
(res <- GetWeekPredCI())
res$pargs$addTarget(gripsWeeklyYR2$enrolled)
scenarios <- list(
  sc1 = GetWeekPredCI(),
  sc2 = GetWeekPredCI(cauchyWt = TRUE),
  sc3 = GetWeekPredCI(fillGaps = TRUE),
  sc4 = GetWeekPredCI(fillGaps = TRUE, coeff = 1.5)
)
maxY <- sapply(scenarios, \(x) x$pargs$maxY) |> max()

graphics::par(mfrow = c(2, 2), oma = c(0, 1, 0, 7), mar = c(4, 4, 3, 1))
for (x in scenarios) x$plot(yMax = maxY, Title = x$call.)

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

bQ <- \(x) substitute(bquote(y), list(y = x)) |> eval() |> as.character()
bQstr <- \(x) (if (is.character(x)) str2lang(x) else x) |> bQ()
unQ <- \(x) vapply(x, bQstr, "")
ss <- \(x) {
  p <- "\\.\\(([^()]*(?:\\((?:[^()]*(?:\\([^()]*\\))*[^()]*)*\\))?[^()]*)*\\)"
  m <- gregexpr(p, x, perl = TRUE)
  if (m[[1L]][[1L]] == -1) return(x)
  regmatches(x, m) <- regmatches(x, m) |> lapply(unQ)
  x
}


mySwitch <- \(x) {
  switch(
    typeof(x),
    character = x,
    symbol = deparse(x),
    language = unQ(x)
  )
}

fn <- \(...) {
  cargs <- as.list(substitute(...()))
  lapply(cargs, mySwitch)
}



