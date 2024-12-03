desc <- "Accurate prediction of subject recruitment for randomized clinical
trials (RCT) remains anongoing challenge. Previous prediction models rely on
parametric assumptions. We present functions for non-parametric RCT recruitment
prediction under several scenarios."

ttl <- "Non-Parametric Recruitment Prediction for Randomized Clinical Trials"
def <- use_description_defaults("RCTrecruit", fields = list(
  Title = ttl,
  Description = gsub("\\n", "", desc),
  lazyData = TRUE,
  Version = "0.1.0"
))

use_description(def)
use_mit_license()
use_package("Rcpp")
use_package("Rcpp", type = "LinkingTo")
use_package("tis")
use_package("lubridate")

aut <- c(
  getOption("usethis.description")[[1]],
  person(
    "Alejandro",
    "Villasante-Tezanos",
    email = "alvillas@utmb.edu",
    role = c("aut")),
  person(
    "Christopher",
    "Kurinec",
    email = "chkurine@utmb.edu",
    role = c("aut")),
  person("Xiaoying", "Yu", email = "xiyu@utmb.edu", role = c("aut"))
)


m <- read.dcf("DESCRIPTION") |>
  as.data.frame() |>
  lapply(\(x) gsub("\\n", " ", x))
m$`Authors@R` <- aut


use_description(m)
the$datWeeks


LoadData(gripsYR1, ScreenDt, Enrolled);
datWeeks <- the$datWeeks

labs <- list(
  week = "Calendar week",
  year = "Calendar year",
  enrolled = "Number of people enrolled that week",
  ActiveDays = "Number of days in that week when recruitment was active"
)

for (x in names(datWeeks)) {
  attr(datWeeks[[x]], "Description") <- labs[[x]]
}



system.file("DESCRIPTION", package = "accrual") |>
  read.dcf() |>
  as.data.frame() |>
  lapply(\(x) gsub("\\n", "", x))




# (base::requireNamespace("knitr", quietly = TRUE)) {


a <- readRDS("C:/Users/iomalaga/AppData/Local/R/win-library/4.4/accrual/Meta/package.rds")




load_all()


dfile <- "../R-package/source/GRIPS code/GRIPS_log_by_day_with_gaps.csv"
grips <- read.csv(dfile)
grips$N.Rows <- NULL
names(grips) <- c("ScreenDt", "Screened", "MetCriteria", "Enrolled")
grips <- grips[grips$Screened > 0, ]
grips$ScreenDt <- as.Date(grips$ScreenDt,  format = "%m/%d/%Y")
grips <- grips[c("ScreenDt", "Enrolled")]

library(lubridate)
minDtyr1 = minDtyr2 <- min(grips$ScreenDt)
year(minDtyr2) = year(minDtyr2) + 1
maxDtyr1 = maxDtyr2 <- minDtyr2 - 1
year(maxDtyr2) = year(maxDtyr2) + 1


gripsYR1 <- grips[grips$ScreenDt <= maxDtyr1 , ]
rownames(gripsYR1) <- NULL

gripsYR2 <- grips[grips$ScreenDt >= minDtyr2 & grips$ScreenDt <= maxDtyr2, ]
rownames(gripsYR2) <- NULL

yr2 <- with(gripsYR2, days2weeks(ScreenDt, Enrolled))$enrolled





grips <- grips |> within(expr = {
  Screening_Date <- Screening_Date |> as.Date(format = "%m/%d/%Y")
  week <- isoweek(Screening_Date)
  year <- year(Screening_Date)
  isholiday <- isHoliday(Screening_Date)
  notactiveenrolm <- `N Rows` == 1
})

form <- cbind(
  Enrolled,
  MetCriteria,
  Screened,
  notactiveenrolm,
  isholiday
) ~ week + year


gripsweek <- grips |> aggregate(form, sum)
grips1sty = gripsweek[2:53, ]
grips2ndy = gripsweek[54:105, ]
gripsy2 = gripsweek[2:105, ]
gripsr = gripsweek[106:143, ]


require(rbenchmark)
loadTrainVector(grips1sty$Sum_Enrolled)


benchmark(
  "sim1wt" = lapply(seq_len(1e4L), \(x) sim1wt(50L)),
  "sim1wt1" = lapply(seq_len(1e4L), \(x) sim1wt1(50L)),
  "simall" = simAll(),
  replications = 1)

sel <- function(trained, drop = FALSE) {
  nms <- names(trained)
  score <-
    1.0 * nchar(gsub("[^%]", "", nms)) +
    1.5 * grepl("%Y", nms, fixed = TRUE) +
    1.6 * grepl("%y(?!%)", nms, perl = TRUE) +
    .31 * grepl("%[Bb]", nms) +
    .30 * grepl("%Om", nms) +
    .30 * grepl("%Op", nms) +
    .32 * grepl("%Ob", nms)
  n0 <- trained != 0
  assign("score", score, envir = globalenv())
  if (drop) {
    score <- score[n0]
    trained <- trained[n0]
  } else {
    score[!n0] <- -100
  }
  assign("nms", names(trained), envir = globalenv())
  assign("trained", trained, envir = globalenv())
  assign("score", score, envir = globalenv())
  names(trained)[order(score, trained, decreasing = TRUE)]
}


target <- days2weeks(
  gripsYR2$ScreenDt |> fixDate(),
  gripsYR2$Enrolled) |>
  _$enrolled

the$setTarget(target)


y <- GetWeekPredCI()


CreatePredCIplotObj <- \(y) {
  dat <- data.frame(cbind(0:52, y)) |> setNames(c("x", "low", "pred", "high"))
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
  mainPlotObj <- list(
    largs = list(
      x = dat$x,
      y = dat$pred,
      type = "n",
      xlab = "Weeks",
      ylab = "Subjects",
      xlim = c(0, 52),
      ylim = c(0, maxY + 1)
    ),
    plot = \() do.call(plot, mainPlotObj$largs)
  )
  CIRegionObj <- list(
    largs = list(
      x = with(dat, c(x,      x[len], rev(  x[-len]))),
      y = with(dat, c(high, low[len], rev(low[-len]))),
      col = "gray90",
      border = "gray90"
    ),
    add = \() do.call(polygon, CIRegionObj$largs)
  )
  gridObj <- list(
    largs = list(
      v = seq(0, 50, by = 5),
      h = seq(0, maxY, by = 10),
      col = "gray70"
    ),
    add = \() do.call(abline, gridObj$largs)
  )

  linesObj <- list(
    vec = c("train", "pred"),
    largs = list(
      train = list(lwd = 2, col = "blue", lab = "Training data"),
      pred = list(lwd = 2, col = "black", lab = "Predicted"),
      target = list(lwd = 2, col = "red", lab = "Target data")
    ),
    add = \() {
      largs <- linesObj$largs
      if (!is.null(target)) vec <- c(vec, "target")
      colVec <- NULL
      labVec <- NULL
      for (ln in vec) {
        vecArgs <- largs[[ln]]
        colVec <- c(colVec, vecArgs$col)
        labVec <- c(labVec, vecArgs$lab)
        vecArgs$lab <- NULL
        list(dat$x, dat[[ln]]) |> c(vecArgs) |> do.call(lines, args = _)
      }
      legend("topleft", legend = labVec, col = colVec, lwd = 2)
    }
  )
  plotPred <- \() {
    oldPar <- par(no.readonly = TRUE)
    on.exit(do.call(par, oldPar), add = TRUE)
    do.call(par, parArgs)
    mainPlotObj$plot()
    CIRegionObj$add()
    gridObj$add()
    linesObj$add()
  }

  rm(y)
  as.list(environment())
}



dev.off()
par(fig = c(3, 3))
y <- GetWeekPredCI(fillGaps = T, coeff = 1.2)
a <- CreatePredCIplotObj(y)
a$plotPred()



x <- 0:52;
len <- length(x)
xs <- c(x, x[len], rev(x[-len]))
ys <- c(y[, 3], y[len, 1], rev(y[-len, 1]))

oldPar <- par(no.readonly = TRUE)
par(fin = c(6, 6), las = 1, cex.axis = 1.2, cex.lab = 1.2);
plot(x, c(0, target), type = "n", asp = 1, xlab = "Weeks", ylab = "Subjects")
polygon(xs, ys, col = "gray90", border = "gray90")
abline(v = seq(0, 50, by = 5), h = seq(0, 40, by = 10), col = "gray70")
lines(x, y[, 2], lwd = 2)
lines(x, c(0, cumsum(the$TrainVector)), col = "blue", lwd = 2)
lines(x, c(0, target), col = "red", lwd = 2)
do.call(par, oldPar)



predPlot <- R6::R6Class("predPlot",
  public = list(
    env = NULL,
    initialize = function(env) {self$env <- env},
    plot = function(y) {
      oldPar <- par(no.readonly = TRUE)
      y <- y |> as.data.frame() |> setNames(c("low", "pred", "high"))
      x <- 0:52;
      train <- c(0, cumsum(self$env$TrainVector))
      len <- length(x)
      xs <- c(x, x[len], rev(x[-len]))
      ys <- c(y$high, y$low[len], rev(y$low[-len]))
      target <- c(0, days2weeks(gripsYR2[[1]], gripsYR2[[2]])$enrolled |> cumsum())
      lns <- list(train, target, y$high);
      maxY <- sapply(lns, max);a
      id <- which.max(maxY)
      maxY <- maxY[id]
      par(fin = c(6, 6), las = 1, cex.axis = 1.2, cex.lab = 1.2);
      plot(x, lns[[id]], type = "n", xlab = "Weeks", ylab = "Subjects")
      polygon(xs, ys, col = "gray90", border = "gray90")
      abline(v = seq(0, 50, by = 5), h = seq(0, maxY + 5, by = 10), col = "gray70")
      lines(x, y[, 2], lwd = 2)
      lines(x, train, col = "blue", lwd = 2)
      lines(x, target, col = "red", lwd = 2)
      do.call(par, oldPar)
    }
  )
)


a <- predPlot$new(the)

refill()
y <- getWeeksPredCI(fillGaps = T)
a$plot(y)


a <- GetWeekPredCI();
class(a) <- c("aek", class(a))

print.aek <- function(x, ...) {
  utils::head(x) |> logPrint()
  utils::tail(x) |> logPrint()
}



#A function to be run first within exported functions to check for user input
checkExportedFunctionsArgs <- \() {
  if (is.null(the$TrainVector)) err(msg$Load, fmt("LoadData", 160))
  fArgs <- getCall(1L)
  for (nam in names(fArgs)) {
    nn <- bold(nam, 160)
    val <- eval(fArgs[[nam]])
    argsTests[[nam]](nn, val)
  }
}


deparseSymbol <- \(y, n = 0L) {
  symbolNames <- c("data", "date", "enrolled")
  for (i in seq_along(y)) {
    if (is.symbol(y[[i]])) {
      str <- deparse(y[[i]])
      if (str == ".") y[[i]] <- eval(y[[i]], parent.frame(3L + n))
      else if (names(y[i]) %in% symbolNames) y[[i]] <- str
      else y[[i]] <- eval(y[[i]])
    }
  }
  y
}

getCall <- \(n = 0L) {
  dArgs <- formals(sys.function(sys.parent(1L + n)))
  defNams <- names(dArgs)
  cArgs <- as.list(sys.call(-1L - n))[-1L]
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
  deparseSymbol(res, n)
}



LoadData(gripsYR1, ScreenDt, Enrolled)
(res <- GetWeekPredCI(fillGaps = TRUE, coeff = 1.5))
scenarios <- list(
  sc1 = GetWeekPredCI(),
  sc2 = GetWeekPredCI(cauchyWt = TRUE),
  sc3 = GetWeekPredCI(fillGaps = TRUE),
  sc4 = GetWeekPredCI(fillGaps = TRUE, coeff = 1.2)
)
maxY <- sapply(scenarios, \(x) x$plot_$maxY) |> max()

graphics::par(mfrow = c(2, 2))
for (x in scenarios) x$predPlot(yMax = maxY, Title = x$call.)

.l <- \(lst, el) {
    paste("\\(x) x$", deparse(substitute(el))) |>
    parse(text = _) |>
    eval() |>
    (\(f) lst |> lapply(f))(f = _)

}


b <- \(y) {
  substitute(x$y)
}


mySwitch <- \(x) {
  switch(
    typeof(x),
    character = x,
    symbol = deparse(x),
    language = (\(y) substitute(bquote(x), list(x = y)) |> eval())(x)
  )
}


a <- quote(I(aek,  .(b)))
substitute(bquote(x), list(x = a)) |> eval()

fn <- \(...) {
cargs <- as.list(substitute(...()))
lapply(cargs, mySwitch)

}
# substitute(bquote(x), list(x = y[[2]])) |> eval()


library(dplyr)
data <- gripsYR1 %>%
  rename(date = ScreenDt,
    enrolled = Enrolled)

test <- \(dat, date, enrolled) {
  getCall()
}
data %>% test(date, enrolled)

target = sample(seq(0, 3), 100, replace = T)


data_ymd <- data %>%
  mutate(date = ymd(date))

data_dym <- data_ymd %>%
  mutate(date = format(date, '%d-%Y-%m'))


set.seed(123)
random_dates <- sample(seq(as.Date("2024-01-01"),
                           as.Date("2024-12-31"),
                           by = "day"),
                        size = 100)

dates_numeric <- as.numeric(format(random_dates, "%Y%m%d"))

set.seed(123)
 random_enrollment <- sample(seq(0, 5), size = 100, replace = T)

data2 <- tibble(
date = dates_numeric,
enrolled = random_enrollment
)

 data2



