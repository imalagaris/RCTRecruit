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

#
CreatePredCIplotObj <- \(y) {
  self <- environment()
  dat <- data.frame(cbind(seq_len(nrow(y)) - 1L, y)) |>
    stats::setNames(c("x", "low", "pred", "high"))
  len <- length(dat$pred)
  dat$train <- c(0, cumsum(the$TrainVector), rep(NA, len - 53))
  if (utils::hasName(the, "target")) {
    dat$target <- c(0, cumsum(the$target), rep(NA, len - 53))
  }
  addTarget <- \(x) {
    the$setTarget(x)
    self$dat$target <- c(0, cumsum(the$target), rep(NA, len - 53))
  }
  getMaxYatWeek <- \(week) {
    max(dat$high[week], dat$target[week], na.rm = TRUE)
  }
  len <- length(dat$pred)
  maxY <- dat$high[[len]]
  parArgs <- list(
    las = 1,
    cex.axis = 1.2,
    cex.lab = 1.2
  )
  main <- list(
    plot = \() do.call(plot, main[-1L]),
    x = dat$x,
    y = dat$pred,
    type = "n",
    xlab = "Weeks",
    ylab = "Subjects",
    xlim = c(0, dat$x[[len]]),
    ylim = c(0, maxY + 1)
  )
  CI95 <- list(
    add = \() do.call(graphics::polygon, CI95[-1L]),
    x = with(dat, c(x,      x[len], rev(  x[-len]))),
    y = with(dat, c(high, low[len], rev(low[-len]))),
    col = "gray90",
    border = "gray90"
  )
  
  grid <- list(
    add = \() do.call(graphics::abline, grid[-1L]),
    v = seq(0, 100, by = 10),
    h = seq(0, maxY, by = 10),
    col = "gray70"
  )
  Xs <- \(y) 0L:(length(dat[[y]]) - 1L)

  lines_ <- list(
    add = \() {
      arg <- lines_[-1L]
      vec <-  c("train", "pred")
      if (!is.null(dat$target)) vec <- c(vec, "target")
      colVec <- NULL
      labVec <- NULL
      for (ln in vec) {
        vArgs <- arg[[ln]]
        colVec <- c(colVec, vArgs$col)
        labVec <- c(labVec, vArgs$lab)
        vArgs$lab <- NULL
        list(x = Xs(ln), y = dat[[ln]], lwd = 3) |> c(vArgs) |>
          do.call(graphics::lines, args = _)
      }
      graphics::legend("topleft", legend = labVec, col = colVec, lwd = 3)
    },
    train  = list(col = "blue",  lab = "Training data"),
    pred   = list(col = "black", lab = "Predicted"),
    target = list(col = "red",   lab = "Target data")
  )

  initArgs <-
    as.list(sys.call(-1L))[-1L] |>
    (\(x) names(x) |> lapply(\(y) paste0(y, " = ", x[[y]])) )() |>
    unlist() |>
    paste0(collapse = ", ") |>
    paste0("(", ... = _, ")") |>
    (\(x) if (x == "()") "Default" else x)()

  initState <- as.list(self)

  setNewMaxYplot <- \(y) {
    self$main$ylim[[2]] <-  y
    self$grid$h <- seq(0, y, by = 10)
  }

  predPlot <- \(yMax = NULL, Title = NULL, aek)  {
    if (!is.null(yMax)) setNewMaxYplot(yMax)
    if (!is.null(Title)) self$main[["main"]] <- Title
    do.call(graphics::par, parArgs)
    main$plot()
    CI95$add()
    grid$add()
    lines_$add()
  }

  reset <- \(yMax = NULL, Title = NULL) {
    for (nm in names(initState)) {
      self[[nm]] <- initState[[nm]]
    }
    predPlot(yMax, Title)
  }
  self
}
