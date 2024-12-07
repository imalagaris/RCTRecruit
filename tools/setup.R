local({
  rp <- "https://cran.rstudio.com/"
  # tinytex::tlmgr_install("makeindex")
  pckgs <- c("Rcpp", "devtools", "usethis", "knitr", "lubridate", "tis", "bibtex")
  chckpg <- \(x) !base::requireNamespace(x, quietly = TRUE)
  pckgs[pckgs |> sapply(chckpg)] |> 
    (\(x) {if (length(x)) {utils::install.packages(x, repos = rp)}})()
  if (!pkgbuild::has_rtools()) {
    Rt <- "https://CRAN.R-project.org/bin/windows/Rtools/"
    cat("\tPlease, click to install Rtools:\n\t\033[32m", 
        cli::style_hyperlink(text = Rt, url = Rt)
    )
  }
})
