# Fit/add cumsum of variable to df.
add2Df <- \(df, v, nm = NULL, dfn = nrow(df)) {
  if (is.null(nm)) nm <- deparse(substitute(v))
  v <- if (is.null(v)) NA_integer_ else c(0L, cumsum(v))
  d = dfn - length(v)
  v <- if (!d) v else if (d < 0) v[seq_len(dfn)] else c(v, rep(NA, d))
  df[[nm]] <- v
  df
}

# Env with plot object data
pDatObj <- \(e) {
  self <- environment()
  MatCI = e$.MatCI
  state0 = NULL
  len = maxY = 0L
  df = Data <- 
    (seq_len(nrow(MatCI)) - 1L) |>
    data.frame(MatCI) |> 
    stats::setNames(c("x", "low", "pred", "high")) |>
    add2Df(the$TrainVector, "train") |>
    (\(x, target = the$target) add2Df(x, target))()
  e$Par   <- list(las = 1, cex.axis = 1.2, cex.lab = 1.2)
  e$main  <- list(type = "n", xlab = "Weeks", ylab = "Subjects")
  e$CI95  <- list(col = "gray90", border = "gray90")
  e$grid  <- list(v = seq(0, 100, by = 10), col = "gray70")
  e$lines <- list(
    train  = list(lty = 1, lwd = 3, col = "blue",  legend = "Training data"),
    pred   = list(lty = 1, lwd = 3, col = "black", legend = "Predicted"),
    target = list(lty = 1, lwd = 3, col = "red",   legend = "Target data")
  )
  self
}

# Env with plot object methods
pFunObj <- \(e) {
  self <- environment()
  .d <- e$.dat
  addTarget <- \(x) {
    the$setTarget(x)
    .d$Data <- .d$Data |> add2Df(x, "target")
    setXY(52)$predPlot()
  }
  setNewMaxYplot <- \(y) {
    e$main$ylim[[2]] <-  y
    e$grid$h <- seq(0, y, by = 10)
  }
  reset <- \(yMax = NULL, Title = NULL) {
    for (nm in names(.d$state0)) {
      e[[nm]] <- .d$state0[[nm]]
    }
    predPlot(yMax, Title)
  }
  gPar <- \(x = e$Par, y = par(names(x))) {
    if (!identical(x, y)) do.call(graphics::par, x) 
  }
  mainPlot = \() do.call(graphics::plot, e$main)
  CI95Add <- \() do.call(graphics::polygon, e$CI95)
  gridAdd <- \() do.call(graphics::abline, e$grid)
  linesAdd <- \() {
    arg <- e$lines |> (\(x) x[vapply(x, \(z) !is.na(z$y[[1L]]), TRUE)])()
    lg <- lapply(arg, `[`,  c("legend", "col", "lwd", "lty")) |> 
      do.call(what = rbind) |> 
      data.frame() |> 
      lapply(unlist) |>
      c(x = "topleft")
    arg <- lapply(arg, \(x) {x$legend <- NULL; x})
    lapply(arg, do.call, what = graphics::lines) |> invisible()
    do.call(graphics::legend, lg)
  }
  predPlot <- \(yMax = NULL, Title = NULL, includeYR2 = FALSE)  {
    if (includeYR2) {
      setXY(104)$predPlot(yMax, Title)
      return(invisible(setXY(52)))
    }
    if (!is.null(yMax)) setNewMaxYplot(yMax)
    if (!is.null(Title)) e$main[["main"]] <- Title
    gPar()
    mainPlot()
    CI95Add()
    gridAdd()
    linesAdd()
  }
  setMain <- \() {
    d <- .d$df
    xy <- list(d$x, d$pred)
    lim <- list(c(0, d$x[[.d$len]]), c(0, e$maxY + 1))
    e$main[c("x", "y", "xlim", "ylim")] <- c(xy, lim)
  }
  setLines <- \() {
    d <- .d$df
    for (n in names(e$lines)) e$lines[[n]][c("x", "y")] <- list(d$x, d[[n]])
  }
  setCI95 <- \() {
    l <- .d$len
    for (n in c("x", "low", "high")) assign(n, .d$df[[n]])
    xx <- c(x, x[l], rev(x[-l]))
    yy <- c(high, low[l], rev(low[-l]))
    e$CI95[c("x", "y")] <- list(xx, yy)
  }
  setXY <- \(n) {
    len = .d$len <- n + 1L;
    df = .d$df <- .d$Data[seq_len(len), ]
    e$maxY <- df$high[len]
    setMain()
    setCI95()
    e$grid$h = seq(0, e$maxY, by = 10)
    setLines()
    if (is.null(.d$state0)) .d$state0 <- as.list(e)
    invisible(self)
  }
  e$reset <- reset
  setXY(52L)
}

# Env for ploting the results of `GetWeekPredCI` function
plotObj <- \(.MatCI) {
  .e <- environment()
  .dat <- pDatObj(.e)
  .fun <- pFunObj(.e)
  rm(.MatCI)
  rm(.e)
  environment()
}
