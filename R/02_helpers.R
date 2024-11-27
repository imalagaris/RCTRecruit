# Initial setup
the <- new.env(parent = emptyenv())
Rcpp::loadModule(module = "mod", TRUE, env =  environment())
load("R/sysdata.rda")
the$binomWt <- wts[["binomial"]]
the$probs <- the$binomWt
the$cauchyWt <- wts[["cauchy"]]
the$color <- .Platform$GUI %in% c("RStudio", "RTerm")

# On data load, export the methods of the C++ module to the `the`
# internal environment
exportModuleMethods <- \(instance) {
  cl <- substitute(instance)
  if (!methods::is(instance, "Rcpp_rct")) stop("For internal use only")
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

# Try parse string to date format
fixDate <- function(dateVar) {
  if (is.null(dateVar)) stop("strDate is NULL")
  type <- typeof(dateVar)
  if (type == "character") {
    fmtDate <- c(
      "Ymd", "Ymd HM", "Ymd HMS", "mdY", "mdY IMp",
      "mdY IMSp", "dmY", "dmY HM", "dmY HMS"
    )
    out <- dateVar |> lubridate::parse_date_time(fmtDate) |> as.Date()
  } else if (type %in% c("integer", "numeric", "double")) {
    out <- as.Date(dateVar, origin = "1970-01-01")
  } else {
    stop("strDate must be a character or numeric vector")
  }
  NAs <- is.na(out)
  if (any(NAs)) {
    ids <- which(NAs)
    len <- length(ids)
    ids <- ids[seq_len(min(10L, len))]
    for (i in ids) {
      vals <- c(msg$idx, lapply(c("strDate", i, dateVar[[i]]), fmt, 208))
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
  enrolled <- as.integer(enrolled)
  checkInvalidValues(enrolled)
  enrolled
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
  dat <-
    data.frame(cbind(0:52, y)) |>
    stats::setNames(c("x", "low", "pred", "high"))
  dat$train <- c(0, cumsum(the$TrainVector))
  if (length(the$cppModule$target)) {
    dat$target <- c(0, cumsum(the$cppModule$target))
  }
  len <- nrow(dat)
  maxY <- dat[len, -c(1:2)] |> max()
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
    xlim = c(0, 52),
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
    v = seq(0, 50, by = 5),
    h = seq(0, maxY, by = 10),
    col = "gray70"
  )

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
        list(dat$x, dat[[ln]]) |> c(vArgs) |> do.call(graphics::lines, args = _)
      }
      graphics::legend("topleft", legend = labVec, col = colVec, lwd = 2)
    },
    train = list(lwd = 2, col = "blue", lab = "Training data"),
    pred = list(lwd = 2, col = "black", lab = "Predicted"),
    target = list(lwd = 2, col = "red", lab = "Target data")
  )
  predPlot <- \() {
    oldPar <- graphics::par(no.readonly = TRUE)
    on.exit(do.call(graphics::par, oldPar), add = TRUE)
    do.call(graphics::par, parArgs)
    main$plot()
    CI95$add()
    grid$add()
    lines_$add()
  }
  rm(y)
  initState <- as.list(self)
  reset <- \() {
    for (nm in names(initState)) {
      self[[nm]] <- initState[[nm]]
    }
    predPlot()
  }
  self
}

#' @export
print.RCTRecruitPredCI <- function(x, ...) {
  print(x$predCI |> utils::head())
  cat("\t", "...", "\n")
  print(x$predCI |> utils::tail())
  invisible(x)
}

#' @export
print.RCTRecruitNWeeks <- function(x, ...) {
  log(msg$enrollWeeks, bold(x$cargs$nSub, 28), bold(x$CI[[2L]], 28))
  print(round(x$CI))
  invisible(x)
}
#' @export
print.RCTRecruitDist <- function(x, ...) {
  print(round(x$CI))
  invisible(x)
}




