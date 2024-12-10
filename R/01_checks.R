# Error messages collect in a named list
msg <- list(
  Load = 'Please, use "%s" function to load your data',
  boolean = "%s must be TRUE or FALSE",
  len = "%s must have length = 1",
  range = "%s must be between %s and %s",
  num = "%s must be a numeric or integer",
  enrollWeeks = "Enrolling %s subjects requires %s weeks\n\n",
  success = "\nVariables %s and %s were successfully loaded\n\n",
  notFound = '"%s" not found in data "%s"\n',
  idx = "%s[[%sL]]: %s"
)
# Simple function to check for user input validity
notScalar <- \(x) length(x) != 1
NoBool <- \(x, y = utils::type.convert(x, as.is = T)) is.na(y) || !is.logical(y)
isNotNum <- \(x) !(typeof(x) %in% c("integer", "double"))

# List of functions to check user input
argsTests <- list(
  "nSim" = \(nam, x) {
    if (isNotNum(x)) err(msg$num, em(nam))
    else if (notScalar(x)) err(msg$len, em(nam))
    else if (x < 1 || x > 1e4) err(msg$range, em(nam), em(1), em(1e4))
  },
  "fillGaps" = \(nam, x) {
    if (NoBool(x)) err(msg$boolean, em(nam))
    else if (notScalar(x)) err(msg$len, em(nam))
  },
  "cauchyWt" = \(nam, x) {
    if (NoBool(x)) err(msg$boolean, em(nam))
    else if (notScalar(x)) err(msg$len, em(nam))
  },
  "nSub" = \(nam, x) {
    maxN <- sum(the$TrainVector) * 10L
    if (isNotNum(x)) err(msg$num, em(nam))
    else if (notScalar(x)) err(msg$len, em(nam))
    else if (x < 1 || x > maxN) err(msg$range, em(nam), em(1), em(maxN))
  },
  "efficiencyFactor" = \(nam, x) {
    if (x < 0.1 || x > 2) err(msg$range, em(nam), em(.1), em(2))
  },
  "target" = \(nam, x) {
    if (isNotNum(x)) err(msg$num, em(nam))
    else if (length(x) < 1) err(msg$len, em(nam))
  }
)

deparseSymbol <- \(y, n = 0L) {
  for (i in seq_along(y)) {
    nam <- names(y[i]) 
    isLoadArg <- nam %in% c("data", "date", "enrolled")
    if (is.symbol(y[[i]])) {
      str <- deparse(y[[i]])
      if (str == ".") y[[i]] <- substitute(., parent.frame(3L + n)) |> deparse()
      else if (isLoadArg) y[[i]] <- str
      else y[[i]] <- eval(y[[i]], parent.frame(3L + n))
    }
    if (isLoadArg & (length(y[[i]]) != 1 | !is.character(y[[i]]))) {
      msg <- "%s must be a variable name (symbol/character) in the input df"
      err(msg, em(nam))
    }
  }
  y
}

getCall <- \(n = 0L) {
  dArgs <- formals(sys.function(sys.parent(1L + n)))
  defNams <- names(dArgs)
  call. = sys.call(-(n + 1L));
  cArgs <- as.list(call.)[-1L]
  out <- list()
  for (nn in defNams) {
    if (utils::hasName(cArgs, nn)) {
      out[[nn]] <- cArgs[[nn]]
      cArgs[[nn]] <- NULL
      dArgs[[nn]] <- NULL
    }
  }
  for (i in seq_along(cArgs)) dArgs[[i]] <- cArgs[[i]]
  res <- c(out, dArgs)[defNams]
  list(call. = deparse(call.), cargs = deparseSymbol(res, n))
}

#A function to be run first within exported functions to check for user input
checkExportedFunctionsArgs <- \() {
  if (is.null(the$TrainVector)) err(msg$Load, fmt("LoadData", 160))
  fArgs <- getCall(1L)$cargs
  for (nam in names(fArgs)) {
    nn <- bold(nam, 160)
    val <- eval(fArgs[[nam]])
    argsTests[[nam]](nn, val)
  }
}

# Check the validity of user input `Date` and 'Enrolled' columns
checkArgs <- function() {
  cargs <- getCall(1L)$cargs
  datN <- cargs$data
  dtN <- cargs$date
  enrN <- cargs$enrolled
  dat <- get(datN, parent.frame())
  for (x in c(dtN, enrN)) {
    if (!utils::hasName(dat, x)) err(msg$notFound, em(x), em(datN))
  }
  date <- fixDate(dat[[dtN]])
  enrolled <- fixEnrolled(dat[[enrN]])
  data.frame(date, enrolled)
}

# Print success message after loading data
LoadSuccess <- \(x) {
  y <- lapply(x, bold, 28)
  log(msg$success, y$enrolled, y$date)
}

checkDate <- function(x) {
  check <- typeof(x) %in% c("character", "integer", "numeric", "double")
  if (!check) err("%s must be a character or numeric vector", em("date"))
}

# Check input is of correct type
checkIntNumType <- function(x) {
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
