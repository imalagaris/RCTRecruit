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


system.file("DESCRIPTION", package = "accrual") |>
  read.dcf() |>
  as.data.frame() |>
  lapply(\(x) gsub("\\n", "", x))


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



bQ <- \(x) substitute(bquote(y), list(y = x)) |> eval() |> as.character()
bQstr <- \(x) (if (is.character(x)) str2lang(x) else x) |> bQ()
unQ <- \(x) vapply(x, bQstr, "")
ss <- \(x) {
  p <- "\\.\\(([^()]*(?:\\((?:[^()]*(?:\\([^()]*\\))*[^()]*)*\\))?[^()]*)*\\)"
  m <- gregexpr(p, x, perl = TRUE)
  if (m[[1L]][[1L]] == -1) return(x)
  regmatches(x, m) <- regmatches(x, m) |> lapply(unQ)
  x
}


mySwitch <- \(x) {
  switch(
    typeof(x),
    character = x,
    symbol = deparse(x),
    language = unQ(x)
  )
}

fn <- \(...) {
  cargs <- as.list(substitute(...()))
  lapply(cargs, mySwitch)
}


fn <- \(l) {
  for (x in names(l)) {
    if (!is.atomic(l[[x]])) {
      if (!is.list(l[[x]]))  l[[x]] <- NULL
      else l[[x]] <- fn(l[[x]])
    }
    else if (is.character(l[[x]])) l[[x]] <- NULL
  }
  return(l)
}
  


fn <- \(l, out = NULL) {
  for (x in names(l)) {
    if (!is.atomic(l[[x]])) {
      if (!is.list(l[[x]]))  l[[x]] <- NULL
      else l[[x]] <- fn(l[[x]], c(out, x))
    }
    else if (is.character(l[[x]])) l[[x]] <- NULL
    else l[[x]] <- paste0(c(out, x), collapse = "$")
  }
  return(l)

}

fn <- \(l) {
  for (x in names(l)) {
    if (!is.atomic(l[[x]])) {
      if (!is.list(l[[x]]))  l[[x]] <- NULL
      else l[[x]] <- paste(x, names(fn(l[[x]])), sep = "$")
    }
    else if (is.character(l[[x]])) l[[x]] <- NULL
  }
  return(l)
}


b <- mainEnv$lines

v <- c("legend", "col", "lwd")


