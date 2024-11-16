desc <- "Accurate prediction of subject recruitment for randomized clinical
trials (RCT) remains anongoing challenge. Previous prediction models rely on
parametric assumptions. We present functions for non-parametric RCT recruitment
prediction under several scenarios."

ttl <- "Non-parametric Recruitment Prediction for Randomized Clinical Trials"
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


LoadData(gripsIM, ScreenDt, Enrolled);
datWeeks <- the$datWeeks

labs <- list(
  week = "Calendar week",
  year = "Calendar year",
  enrolled = "Number of people enrolled that week",
  holiday = "Number of federal holidays that week",
  cnt = "Number of days in that week when recruitment was active"
)

for (x in names(datWeeks)) {
  attr(datWeeks[[x]], "Description") <- labs[[x]]
}


dpath <- system.file("DESCRIPTION", package = "accrual")

read.dcf(dpath) |> as.data.frame() |> lapply(\(x) gsub("\\n", "", x))



# (base::requireNamespace("knitr", quietly = TRUE)) {







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


target <- days2weeks(gripsYR2$ScreenDt, gripsYR2$Enrolled)$enrolled |> cumsum()


y <- PredCI(nSim = 1e4, fillGaps = F, cauchyWt = T)
x <- 0:52;
len <- length(x)
xs <- c(x, x[len], rev(x[-len]))
ys <- c(y[, 3], y[len, 1], rev(y[-len, 1]))

par(fin = c(6, 6), las = 1, cex.axis = 1.2, cex.lab = 1.2);
plot(x, c(0, target), type = "n", asp = 1, xlab = "Weeks", ylab = "Subjects")
polygon(xs, ys, col = "gray90", border = "gray90")
abline(v = seq(0, 50, by = 5), h = seq(0, 40, by = 10), col = "gray70")
lines(x, y[, 2], lwd = 2)
lines(x, c(0, cumsum(the$TrainVector)), col = "blue", lwd = 2)
lines(x, c(0, target), col = "red", lwd = 2)



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




load_all()
oldPar <- par(no.readonly = TRUE)
LoadData(gripsIM, ScreenDt, Enrolled)
train <- the$TrainVector
the$datWeeks[1:5, ]; cat("...\n"); the$datWeeks[48:52, ]
nonGap <- the$datWeeks$cnt != 0
gapIdx <- which(!nonGap)
the$datWeeks[gapIdx, ]
nonGapVals <- train[nonGap]


bnWt <- stats::dbinom(0L:51L, 51L, 0.5)
idWt <- \(t) ((0L:51L + 26L - t) %% 52L) + 1L
getWts <- \(t) (bnWt[idWt(t)] * nonGap) |> (\(x) x / sum(x))()
p <- lapply(gapIdx, getWts) |> setNames(paste("Week", gapIdx))



gPar <- list(
  fig = c(0, .5, 0, 1), 
  mar = c(0, 4, 1.2, 0), 
  oma = c(2,2,2,1),  
  pty = "s",
  las = 1, 
  cex.axis = 1.2, 
  cex.lab = 1.2, 
  font.lab = 2,
  new = FALSE
)

plotPar <- list(
  x = p[["Week 20"]],
  type = "h", 
  xlab = "", 
  ylab = "Sampling Weights",
  main = "Week 20",
  lwd = 3,
  ylim = c(0, 0.55)
)


addLnPnt <- \(x) {
  points(x, 0, cex = 1.2, pch = 19, col = "red")
  abline(h = c(1:5) / 10, lty = 2, col = "gray80")
}

do.call(par, gPar)
do.call(plot, plotPar)
addLnPnt(20)

gPar[c("fig", "new")] = list(c(.5, 1, 0, 1), TRUE)
plotPar[c("x", "main", "ylab")] = list(p[["Week 28"]], "Week 28", "")
do.call(par, gPar)
do.call(plot, plotPar)
addLnPnt(28)

mtext("Weights for Filling Gaps", 3, -2, TRUE, cex = 1.5, font = 2)
mtext("Weeks", 1, 0, TRUE, cex = 1.2, font = 2)

do.call(par, oldPar)


len <- length(gapIdx)
wk <- seq_len(len) |> setNames(names(p))

btstrp <- \() vapply(wk, \(x) sample(nonGapVals, 1), 0L)
bnm <- \(n = 1) vapply(wk, \(x) mean(sample(train, n, FALSE, p[[x]])), .0)
list2df <- \(x) do.call(rbind, x) |> as.data.frame()

nSim <- seq_len(1e4)
dfs <- list()
dfs[["Bootstrap"]] <- lapply(nSim, \(x) btstrp()) |> list2df()
dfs[["Binom"]]     <- lapply(nSim, \(x) bnm(1L))  |> list2df()
dfs[["BinomMu3"]]  <- lapply(nSim, \(x) bnm(3L))  |> list2df()
dfs[["BinomMu6"]]  <- lapply(nSim, \(x) bnm(6L))  |> list2df()
dfs[["BinomMu9"]]  <- lapply(nSim, \(x) bnm(9L))  |> list2df()


out <- numeric(len) |> setNames(names(p))
reportMeanCVUnit <- \(x) sprintf("%5.2f (%3.1f)", mean(x), sd(x) / mean(x))
reportMeanSdUnit <- \(x) sprintf("%5.2f (%3.1f)", mean(x), sd(x))
reportMeanCV <- function(df) {
  out <- lapply(df, reportMeanCVUnit) |> cbind()
  N <- ((apply(df, 1, sum)) + 18) |> reportMeanSdUnit()
  rbind(out, N)
}

for (i in seq_len(len)) out[i] <- sum(train * p[[i]])
WtMean <- c(out, 18 + sum(out)) |> sprintf(fmt = "%5.2f", ... = _)
MeanCV <- lapply(dfs, reportMeanCV) |> as.data.frame()

cbind(WtMean, MeanCV)
lapply(dfs, \(x) { round(rowSums(x) + 18) |>  quantile(c(.025, .5, .975))}) |> 
  do.call(rbind, args = _)









newFill <- \(n = 1, repl. = FALSE) {
  indxWt <- \(t) ((0L:51L + 26L - t) %% 52L) + 1L
  bn <- the$binomWt[[26]]
  id0 = which(the$datWeeks$cnt == 0)
  p <- lapply(1L:52L, \(t) bn[indxWt(t)] |> (\(x) {x[id0] <- 0; x / sum(x)})())
  out = train = the$TrainVector
  for (wk in id0) out[wk] <- mean(sample(train, n, repl., p[[wk]]))
  out
}


newFill1 <- \(x) {
  out <- the$TrainVector;
  out <- setNames(numeric(length(id0)), paste0("w", id0))
  for (i in seq_len(length(id0))) { 
    wk <- id0[i]
    out[i] <- mean(sample(the$TrainVector, 8, replace = FALSE, prob = p[[wk]]))
  }
  out
}

gaps <- lapply(1:1000, newFill1) |> do.call(rbind, args = _) |> as.data.frame()
one <- lapply(gaps, \(x) sd(x)/ mean(x)) |> cbind()

eight <- lapply(gaps, \(x) sd(x)/ mean(x)) |> cbind()



fill1 <- \(x) {
  refill() |> capture.output() |> invisible()
  sum(the$Trainfilled)
}

fill2 <- \(x) {
  newFill() |> sum() |> round()
}

f1 <- vapply(seq_len(1e4), fill1, 0L)
f2 <- vapply(seq_len(1e4), fill2, 1)

tab1 <- table(f1)
tab2 <- table(f2)

f2 |> quantile()


wk <- 15
p0 <- p[indxWt(wk)]
p0[id0] <- 0
p0 <- p0 / sum(p0)
cbind(the$TrainVector, round(p0 * 1000, 3), p0 == 0)


binomWt <- stats::dbinom(0L:51L, 51L, 0.5) |> (\(x) x / sum(x))()

id0 = which(the$datWeeks$cnt == 0)

getCall <- \(n = 0L) {
  deparseSymbol <- \(y) if (is.symbol(y)) deparse(y) else y
  carg <- formals(sys.function(sys.parent()))
  cl <- as.list(sys.call(-1L-n))[-1L]
  for (nn in names(carg)) {
    if (hasName(cl, nn)) {
      carg[[nn]] <- deparseSymbol(cl[[nn]])
      cl[[nn]] <- NULL
    }
  }
  id <- carg |> vapply(is.symbol, logical(1L)) |> which()
  for (i in seq_along(id)) carg[[id[[i]]]] <- deparseSymbol(cl[[i]])
  carg
}
aek(a = a1, d = d1, b1 , e1)

aek <- function(a, b, d, e, fuck = TRUE) {
  getCall()
}

