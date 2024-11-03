local({
  pckgs <- c("Rcpp", "devtools", "usethis", "knitr", "lubridate", "tis", "bibtex")
  chckpg <- \(x) !base::requireNamespace(x, quietly = TRUE)
  if (length(pckgs[pckgs |> sapply(chckpg)])) {
    cat(
      "\n\n\033[32mPlease, execute in console the following command:\n",
      '\033[1;31m  source("setup.R")'
    )
  }
})

if (base::requireNamespace("devtools", quietly = TRUE)) {
  if (interactive()) {
    suppressMessages(require(devtools))
  }
  source("tools/dataDocHelpers.R");
  options( usethis.overwrite = TRUE )
}
