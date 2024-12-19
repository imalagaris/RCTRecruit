# Initial setup
the <- new.env(parent = emptyenv())
Rcpp::loadModule(module = "mod", TRUE, env =  environment())
load("R/sysdata.rda")
the$binomWt <- wts[["binomial"]]
the$probs <- the$binomWt
the$cauchyWt <- wts[["cauchy"]]
the$color <- .Platform$GUI %in% c("RStudio", "RTerm")

the$initPar <- graphics::par(no.readonly = TRUE)
the$resetPar <- \() do.call(graphics::par, the$initPar)

# On data load, export the methods of the C++ module to the `the`
# internal environment
exportModuleMethods <- \(instance) {
  cl <- substitute(instance)
  mn <- rct@methods |> names()
  mthds <- lapply(mn, as.name)
  fx <- \(x, y) {
    lapply(y, \(z) {
      z <- z
      x <- x
      substitute(x$z)
    })
  }
  fx(cl, mthds) |> lapply(eval, topenv())
  lapply(mn, \(m) the[[m]] <- get(m, instance)) |> invisible()
}

# Called within exported functions to set the training data as filled or not
useFilled <- function(fill = FALSE, env = the) {
  train <- if (fill) env$Trainfilled else env$TrainVector
  the$cppModule$train <- train
  env$train <- train
}

# Called within exported functions to set the efficiency coefficient
applyCoeff <- function(coeff) {
  if (coeff != 1) {
    the$train <- the$train * coeff
    the$cppModule$train <- the$train
  }
}

# Fill gap weeks with its expected value
fillEmptyWeeks <- \() {
  train <- the$datWeeks$enrolled
  gapIdx <- which(the$datWeeks$activeDays == 0)
  out <- train
  for (i in gapIdx) {
    wt <- the$binomWt[[i]]
    wt[gapIdx] <- 0
    wt <- wt / sum(wt)
    out[i] <- sum(train * wt)
  }
  out
}

str2Date <- \(x) {
  fmtDate <- c(
    "Ymd", "Ymd HM", "Ymd HMS", "mdY", "mdY IMp",
    "mdY IMSp", "dmY", "dmY HM", "dmY HMS"
  )
  x <- if (is.character(x)) x else as.character(x)
  lubridate::parse_date_time(x, fmtDate) |> as.Date()
}

# Try parse string to date format
fixDate <- function(x) {
  checkDate(x)
  out <- str2Date(x)
  NAs <- is.na(out)
  if (any(NAs)) {
    ids <- which(NAs)
    len <- length(ids)
    ids <- ids[seq_len(min(10L, len))]
    for (i in ids) {
      vals <- c(msg$idx, lapply(c("strDate", i, x[[i]]), fmt, 208))
      do.call(wrn, vals)
    }
    if (len > 10) warning("...")
    stop("Invalid date values")
  }
  out[order(out)]
}

# Fix the input vector to be integer and without invalid values
fixEnrolled <- function(enrolled) {
  checkIntNumType(enrolled)
  checkInvalidValues(enrolled)
  as.integer(enrolled)
}

# Aggregate the data by week and year
days2weeks <- function(date, enrolled) {
  dat <- data.frame(date, enrolled)
  nn <- length(date)
  gaps <- c(as.integer(diff(date)) - 1L, 0L)
  slen <- seq_len(sum(gaps))
  dlist <- lapply(slen, \(x) list(date = NULL, enrolled = integer(1L)))
  j <- 0L
  for (i in seq_len(nn)) {
    gap <- gaps[[i]]
    if (gap) {
      for (k in seq_len(gap)) {
        j <- j + 1L
        dlist[[j]][["date"]] <- date[[i]] + k
      }
    }
  }
  cnt <- stats::setNames(integer(53L), seq_len(53L))
  tab <- table(lubridate::isoweek(date))
  cnt[names(tab)] <- tab
  dlist <- do.call(rbind.data.frame,  dlist)
  dat <- rbind(dat, dlist)
  dat <- dat[order(dat$date), ]
  dat$week <- lubridate::isoweek(dat$date)
  dat$year <- lubridate::isoyear(dat$date)
  datw <- stats::aggregate(enrolled ~ week + year, dat, sum)
  datw$activeDays <- 0L
  datw$activeDays <- cnt[datw$week]
  rownames(datw) <- NULL
  return(datw[1L:52L, ])
}

addDfCol <- \(df, v, nm = NULL, dfn = nrow(df), d = dfn - length(v)) {
  if (is.null(nm)) nm <- deparse(substitute(v))
  v <- if (!d) v else if (d < 0) v[seq_len(dfn)] else c(v, rep(NA, d))
  df[[nm]] <- v
  df
}

#
CreatePredCIplotObj <- \(y) {
  .s <- environment()
  isInit <- !is.data.frame(y)
  if (isInit) {
    Par <- list(
      las = 1,
      cex.axis = 1.2,
      cex.lab = 1.2
    )
    .d <- list()
    .d$y <- y
    df <- data.frame(cbind(seq_len(nrow(y)) - 1L, y)) |>
      stats::setNames(c("x", "low", "pred", "high")) |> 
      addDfCol(c(0, cumsum(the$TrainVector)), "train")
    if (utils::hasName(the, "target")) {
      df <- df |> addDfCol(c(0, cumsum(the$target)), "target")
    }
    .d$df <- df
    .f <- list()
    .f$addTarget <- \(x) {
      the$setTarget(x)
      .s$.d$df <- .s$.d$df |> addDfCol(c(0, cumsum(the$target)), "target")
      .s <- .s$.f$selectRange(52)
    }
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
    .f$setNewMaxYplot <- \(y) {
      .s$main$ylim[[2]] <-  y
      .s$grid$h <- seq(0, y, by = 10)
    }

    .f$mainPlot = \() do.call(plot, .s$main)
    .f$CI95Add <- \() do.call(graphics::polygon, .s$CI95)
    .f$gridAdd <-  \() do.call(graphics::abline, .s$grid)
    .f$linesAdd <- \() {
      arg <- .s$lines |> (\(x) x[vapply(x, \(z) !is.null(z$y), TRUE)])()
      colVec <-  vapply(arg, \(x) x$col, "")
      labVec <-  vapply(arg, \(x) x$lab, "")
      arg <- lapply(arg, \(x) {x$lab <- NULL; c(x, lwd = 3)})
      lapply(arg, do.call, what = graphics::lines) |> invisible()
      graphics::legend("topleft", legend = labVec, col = colVec, lwd = 3)
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
  else df <- y
  
  len <- length(df$pred)
  maxY <- df$high[[len]]
  main <- list(
    x = df$x,
    y = df$pred,
    type = "n",
    xlab = "Weeks",
    ylab = "Subjects",
    xlim = c(0, df$x[[len]]),
    ylim = c(0, maxY + 1)
  )
  
  CI95 <- list(
    x = c(df$x,    df$x[len],   rev(df$x[-len])),
    y = c(df$high, df$low[len], rev(df$low[-len])),
    col = "gray90",
    border = "gray90"
  )
  
  grid <- list(
    v = seq(0, 100, by = 10),
    h = seq(0, .s$maxY, by = 10),
    col = "gray70"
  )
  
  lines <- list(
    train  = list(x = df$x, y = df$train, col = "blue", lab = "Training data"),
    pred   = list(x = df$x, y = df$pred, col = "black", lab = "Predicted"),
    target = list(x = df$x, col = "red", lab = "Target data",
      y = if (is.null(df$target)) NULL else df$target)
  )
  
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
