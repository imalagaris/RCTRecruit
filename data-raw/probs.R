## code to prepare `probs` dataset goes here
indxWt <- \(t) ((0L:51L + 26L - t) %% 52L) + 1L
binomWt <- stats::dbinom(0L:51L, 51L, 0.5) |> (\(x) x / sum(x))()
cauchyWt <- seq(-3L, 3L, 6 / 51) |> stats::dcauchy() |> (\(x) x / sum(x))()
wts <- list()
wts[["binomial"]] <- lapply(1L:52L, \(t) binomWt[indxWt(t)])
wts[["cauchy"]] <- lapply(1L:52L, \(t) cauchyWt[indxWt(t)])
source("tools/dataDocHelpers.R")

usethis::use_data(probs, wts, e, overwrite = TRUE, internal = TRUE)


