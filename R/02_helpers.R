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
    msg <- "%s[[%sL]]: %s"
    for (i in ids) {
      vals <- c(msg, lapply(c(the$dtStr, i, dateVar[[i]]), fmt, 208))
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
