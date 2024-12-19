LoadData(gripsYR1, ScreenDt, Enrolled)
res <- GetWeekPredCI(fillGaps = T, efficiencyFactor = 1.2)

add2Df <- \(df, v, nm = NULL, dfn = nrow(df)) {
  if (is.null(nm)) nm <- deparse(substitute(v))
  v <- if (is.null(v)) NA_integer_ else c(0L, cumsum(v))
  d = dfn - length(v)
  v <- if (!d) v else if (d < 0) v[seq_len(dfn)] else c(v, rep(NA, d))
  df[[nm]] <- v
  df
}

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
    train  = list(lwd = 3, col = "blue",  legend = "Training data"),
    pred   = list(lwd = 3, col = "black", legend = "Predicted"),
    target = list(lwd = 3, col = "red",   legend = "Target data")
  )
  self
}

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
  gPar <- \(x = e$Par, y = par(names(x))) {
    if (!identical(x, y)) do.call(graphics::par, x) 
  }
  mainPlot = \() do.call(graphics::plot, e$main)
  CI95Add <- \() do.call(graphics::polygon, e$CI95)
  gridAdd <- \() do.call(graphics::abline, e$grid)
  linesAdd <- \() {
    arg <- e$lines |> (\(x) x[vapply(x, \(z) !is.na(z$y[[1L]]), TRUE)])()
    lgcol <-  vapply(arg, \(x) x$col, "")
    lglab <-  vapply(arg, \(x) x$legend, "")
    arg <- lapply(arg, \(x) {x$legend <- NULL; x})
    lapply(arg, do.call, what = graphics::lines) |> invisible()
    graphics::legend("topleft", legend = lglab, col = lgcol, lwd = 3)
  }
  predPlot <- \() {
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
  setXY(52L)
}




mainEnv <- 
  res$predCI |> 
  (\(.MatCI) {
    .e <- environment()
    .dat <- pDatObj(.e)
    .fun <- pFunObj(.e)
    rm(.MatCI)
    .e
  })()







aek <- \(y) {

    .f$selectRange <- \(n) {
      s <- seq_len(n + 1L)
      newDat <- .s$.d$df[s, ]
      newE <- CreatePredCIplotObj(newDat)
      .s$maxY <- newE$maxY
      c("x", "y", "xlim", "ylim") |> (\(x) .s$main[x] <- newE$main[x])()
      c("x", "y") |> (\(x) .s$CI95[x] <- newE$CI95[x])()
      c("v", "h") |> (\(x) .s$grid[x] <- newE$grid[x])()
      for (vec in names(.s$lines)) {
        .s$lines[[vec]]$x <- newE$lines[[vec]]$x
        .s$lines[[vec]]$y <- newE$lines[[vec]]$y
      }
      invisible(.s)
    }
    
    
    .f$predPlot <- \(yMax = NULL, Title = NULL, includeYR2 = FALSE)  {
      if (includeYR2) {
        .s$.f$selectRange(104)$.f$predPlot(yMax, Title)
        .s <- .s$.f$selectRange(52)
        return(invisible(NULL))
      }
      if (!is.null(yMax)) .f$setNewMaxYplot(yMax)
      if (!is.null(Title)) .s$main[["main"]] <- Title
      do.call(graphics::par, .s$Par)
      .f$mainPlot()
      .f$CI95Add()
      .f$gridAdd()
      .f$linesAdd()
    }
  }

  
  
  if (isInit) {
    .s <- .s$.f$selectRange(52)
    .d$initArgs <-
      as.list(sys.call(-1L))[-1L] |>
      (\(x) names(x) |> lapply(\(y) paste0(y, " = ", x[[y]])) )() |>
      unlist() |>
      paste0(collapse = ", ") |>
      paste0("(", ... = _, ")") |>
      (\(x) if (x == "()") "Default" else x)()
    
    .d$init <- as.list(.s)[c("maxY", "main", "CI95", "grid", "lines", "Par")]
    reset <- \(yMax = NULL, Title = NULL) {
      for (nm in names(.d$init)) {
        .s[[nm]] <- .d$init[[nm]]
      }
      .s$.f$predPlot(yMax, Title)
    }
    rm(y, df, envir = .s)
  }
  rm(isInit, len, envir = .s)
  .s
}






#














