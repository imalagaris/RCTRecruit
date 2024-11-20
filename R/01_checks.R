# Error messages collect in a named list
msgL <- list(
  Load = 'Please, use "%s" function to load your data',
  boolean = "%s must be TRUE or FALSE",
  len = "%s must have length = 1",
  range = "%s must be between %s and %s",
  num = "%s must be a numeric or integer",
  enrollWeeks = "Enrolling %s subjects requires %s weeks\n\n",
  success = "Variables %s and %s were successfully loaded from data %s\n\n",
  notFound = '"%s" not found in data "%s"\n'
)
# Simple function to check for user input validity
notScalar <- \(x) length(x) != 1
isNotBool <- \(x) !is.logical(x)
isNotNum <- \(x) !(typeof(x) %in% c("integer", "double"))

# List of functions to check user input
argsTests <- list(
  "nSim" = \(nam, x) {
    if (isNotNum(x)) err(msgL$num, nam)
    else if (notScalar(x)) err(msgL$len, nam)
    else if (x < 1 || x > 1e4) {
      low <- bold(1, 160)
      high <-  bold(1e4, 160)
      err(msgL$range, nam, low, high)
    }
  },
  "fillGaps" = \(nam, x) {
    if (isNotBool(x)) err(msgL$boolean, nam)
    else if (notScalar(x)) err(msgL$len, nam)
  },
  "cauchyWt" = \(nam, x) {
    if (isNotBool(x)) err(msgL$boolean, nam)
    else if (notScalar(x)) err(msgL$len, nam)
  },
  "nSub" = \(nam, x) {
    maxN <- sum(the$TrainVector) * 10L
    if (isNotNum(x)) err(msgL$num, nam)
    else if (notScalar(x)) err(msgL$len, nam)
    else if (x < 1 || x > maxN) {
      low <- bold(1, 160)
      high <-  bold(maxN, 160)
      err(msgL$range, nam, low, high)
    }
  },
  "coeff" = \(nam, x) {
    if (x < 0.1 || x > 2) err("coeff must be between 0.1 and 2")
  },
  "target" = \(nam, x) {
    if (isNotNum(x)) err(msgL$num, nam)
    else if (length(x) < 1) err(msgL$len, nam)
  }
)

#A function to be run first within exported functions to check for user input
checkExportedFunctionsArgs <- \() {
  if (is.null(the$TrainVector)) err(msgL$Load, fmt("LoadData", 160))
  fArgs <- getCall(1L)
  for (nam in names(fArgs)) {
    nn <- bold(nam, 160)
    val <- eval(fArgs[[nam]])
    print(typeof(val))
    argsTests[[nam]](nn, val)
  }
}

#Gets the arguments of the calling function
getCall <- \(n = 0L) {
  deparseSymbol <- \(y) if (is.symbol(y)) deparse(y) else y
  dArgs <- formals(sys.function(sys.parent(1L + n)))
  defNams <- names(dArgs)
  cArgs <- as.list(sys.call(-1L - n))[-1L]
  out <- list()
  for (nn in defNams) {
    if (utils::hasName(cArgs, nn)) {
      out[[nn]] <- deparseSymbol(cArgs[[nn]])
      cArgs[[nn]] <- NULL
      dArgs[[nn]] <- NULL
    }
  }
  for (i in seq_along(cArgs)) dArgs[[i]] <- deparseSymbol(cArgs[[i]])
  c(out, dArgs)[defNams]
}

# Check the validity of user input `Date` and 'Enrolled' columns
checkArgs <- function(name) {
  cargs <- getCall(1L)
  dat <- get(cargs$data, parent.frame())
  dataStr <- cargs$data
  varStr <- eval(substitute(cargs$name))
  if (!utils::hasName(dat, varStr)) {
    err(msgL$notFound, bold(varStr, 160), bold(dataStr, 160))
  }
  out <- dat[[varStr]]
  if (deparse(substitute(name)) == "enrolled") fixEnrolled(out)
  else fixDate(out)
}

# Print success message after loading data
LoadSuccess <- \(x) {
  y <- lapply(x, bold, 28)
  log(msgL$success, y$enrolled, y$date, y$data)
}

# Check input is of correct type
checkIntNumType <- function(x) {
  if (is.null(x)) stop("x is NULL")
  if (!(class(x) %in% c("integer", "numeric")))
    stop("x must be an integer/numeric vector")
}

# Check for invalid input values
checkInvalidValues <- function(x) {
  if (any(is.nan(x))) stop("x has NaN values")
  if (any(is.infinite(x))) stop("x has Inf values")
  if (any(is.na(x))) stop("x has NAs")
  if (any(x < 0L)) stop("x has negative values")
}


