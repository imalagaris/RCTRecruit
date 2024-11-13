# Initial setup
the <- new.env(parent = emptyenv())
Rcpp::loadModule(module = "mod", TRUE, env =  environment())
load("R/sysdata.rda")
the$probs = the$binomWt <- wts[["binomial"]]
the$cauchyWt <- wts[["cauchy"]] 
the$color <- .Platform$GUI %in% c("RStudio", "RTerm")

exportModuleMethods <- \(instance) {
  cl <- substitute(instance);
  if (!methods::is(the$cppModule, "Rcpp_rct")) stop("For internal use only")
  mn <- rct@methods |> names()
  mthds <- lapply(mn, as.name)
  fx <- (\(x, y) lapply(y, \(z) {z <- z; x <- x; substitute(x$z)}))
  fx(cl, mthds) |> lapply(eval, topenv())
  lapply(mn, \(m) the[[m]] <- get(m, instance)) |> invisible()
}


useFilled <- function(fill = FALSE, env = the) {
  train <- if (fill) env$Trainfilled else env$TrainVector
  the$cppModule$train = env$train <- train
}

applyCoeff <- function(coeff) {
  if (coeff != 1) {
    the$train = the$cppModule$train <- the$train * coeff;
  }
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
fixEnrolled <- function(TrainVector) {
  checkIntNumType(TrainVector)
  TrainVector <- as.integer(TrainVector)
  checkInvalidValues(TrainVector)
  TrainVector
}

# Aggregate the data by week
#' @export
days2weeks <- function(date, enrolled) {
  dat <- data.frame(date, enrolled);
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
  dat <- within(dat, {
    week <- lubridate::isoweek(date) # nolint: object_usage_linter.
    year <- lubridate::isoyear(date) # nolint: object_usage_linter.
    holiday <- tis::isHoliday(date, TRUE, TRUE) * 1L # nolint
  })
  datw <- stats::aggregate(cbind(enrolled, holiday) ~ week + year, dat, sum)
  datw$cnt <- 0L
  datw$cnt <- cnt[datw$week]
  rownames(datw) <- NULL
  
  return(datw[1L:52L, ])
}

# Fill gap weeks with values sampled from non-zero weeks
fillEmptyWeeks <- function(x, id0) {
  zeroIdx <- which(id0 == 0)
  nonZeroVals <- x[-zeroIdx]
  for (i in zeroIdx) {
    x[i] <- sample(nonZeroVals, 1)
  }
  x
}

refill <- function() {
  pre <- sum(the$Trainfilled);
  zeroIdx <- which(the$datWeeks$cnt == 0)
  nonZeroVals <- the$TrainVector[-zeroIdx]
  for (i in zeroIdx) {
    the$Trainfilled[i] <- sample(nonZeroVals, 1)
  }
  post <- sum(the$Trainfilled);
  cat(pre, "->", post, "\n")
}








