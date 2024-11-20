# Utilities to print custom notes, warnings, and errors to the console
log <- \(x, ...) do.call(sprintf, c(x, list(...))) |> cat()
err <- \(x, ...) do.call(sprintf, c(x, list(...))) |> stop(call. = FALSE)
wrn <- \(x, ...) do.call(sprintf, c(x, list(...))) |>
  warning(call. = FALSE, immediate. = TRUE)
# Utility to print to console and capture output
logPrint <- \(x) print(x) |>
  utils::capture.output() |>
  cat(... = _, "", sep = "\n")

# Utility to check if we are in a markdown document
isMarkdown <- \() {
  if (base::requireNamespace("knitr", quietly = TRUE)) {
    knitr::is_html_output() || knitr::is_latex_output()
  } else {
    FALSE
  }
}
# Utility to colorize text foreground, background, and effects
fmt <- \(str, fg = 0, bk = 0, fx = NULL) {
  if (!the$color || isMarkdown()) return(str)
  larg <- list(
    if (is.null(fx)) fx else sprintf("\033[%sm", fx),
    if (fg) sprintf("\033[38;5;%sm", fg) else NULL,
    if (bk) sprintf("\033[48;5;%sm", bk) else NULL,
    str,
    "\033[0m"
  )
  do.call(paste0, larg)
}
# Colorize text
bold <- \(str, foreground) fmt(str, fg = foreground, fx = 1)
