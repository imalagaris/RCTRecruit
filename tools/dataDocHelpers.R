e <- (\() {
  self <- environment()
  exportDoc <- \(path = "tools/doc") {
    Rds <- list.files("man", full.names = TRUE)
    o <- gsub("Rd$", "txt", basename(Rds)) |> file.path(path, ... = _)
    for (i in seq_along(Rds)) {
      tools::Rd2txt(Rds[[i]], package = "RCTRecruit") |>
        capture.output() |>
        gsub("_\b", "", x = _) |>
        paste0(collapse = "\n") |>
        writeLines(o[[i]])
    }
  }

# Datasets ---------------------------------------------------------------------
  getDatasetInfo <- \(df) {
    rows <- nrow(df)
    cols <- length(df)
    vars <- vector("list", cols)
    for (i in seq_len(cols)) {
      vars[[i]] <- list(
        i = as.character(i),
        name = names(df[i]),
        type = typeof(df[[i]]),
        desc = attr(df[[i]], "Description")
      )
    }
    list(rows = rows, cols = cols, vars = vars)
  }

  genDataDoc <- \(df) {
    dataSizeFmt <- "A data frame with **%d** observations of **%d** variables  "
    varfmt <- \(x) do.call(sprintf, c("| \\[,%s\\] | %s | `%s` | %s |", x))
    datHeader <- c(
      "  ",
      "|      |      |      |      |",
      "| :--- | :--- | :--- | :--- |"
    )
    l <- getDatasetInfo(df)
    size <- sprintf(dataSizeFmt, l$rows, l$cols)
    vars <- lapply(l$vars, varfmt)
    c(size, datHeader, vars) |> unlist(use.names = F) |> paste0(collapse = "\n")
  }

# Rd Tags ----------------------------------------------------------------------
  readDesc <- \(x = NULL) base::read.dcf("DESCRIPTION", x, keep.white = "Date")

  getRdNameTitle <- \(Rd) {
    y = list(docType = NULL, name = NULL, title = NULL)
    for (x in Rd) {
      tag <- attr(x, "Rd_tag") |> gsub("\\\\", "", x = _)
      if (utils::hasName(y, tag)) {
        y[[tag]] <- unlist(x, use.names = F) |> paste0(collapse = "\n")
        if (tag == "title") break;
      }
    }
    if (is.null(y$docType)) y$docType <- "func"
    y$title <- y$title |> gsub("\n", "", x = _)
    val <- sprintf("| | [%s] | %s |", y$name, y$title)
    out <- list(name = y$name, type = y$docType, val = val)
    class(out) <- "data.frame"
    attr(out, "row.names") <- 1L
    out
  }

  exportedTab <- \() {
    l <- list(
      func = list(func = "| Functions: | | |"),
      data = list(data = "| Datasets:  | | |")
    )
    for (Rd in list.files("man", full.names = TRUE)) {
      x <- tools::parse_Rd(Rd) |> getRdNameTitle()
      if (x$type == "package") next;
      l[[x$type]][[x$name]] <- x$val
   }
    unlist(l, use.names = F) |> paste0(collapse = "\n")
  }

  getPackageDetails <- \() {
    fmtDetails <- \(nam, val) sprintf("| %s: | %s | |", nam, val)
    fields <- c("Package", "Type", "Version", "Date", "License")
    header <- c(
      "|      |      |      |",
      "| :--- | :--- | :--- |"
    )
    pack <- readDesc(fields) |> as.data.frame()
    pack$Date <- Sys.Date()
    details <- vapply(names(pack), \(x) fmtDetails(x, pack[[x]]), "")
    c(header, details, exportedTab()) |> paste0(collapse = "\n")
  }

# References -------------------------------------------------------------------
  citationPath <- "tools/citations/"
  fbib <- \(x) paste0(citationPath, x, ".bib")
  addRef <- \(x) fbib(x) |> bibtex::read.bib() |> utils:::format.bibentry()

  mendeley2bib <- \(fname) {
    ref <- readClipboard()
    if (grepl("^@(article|Manual)", ref[[1L]])) {
      if (length(ref) > 4L) {
        write(ref, file = fbib(fname))
        bibs <- list.files(citationPath, "*\\.bib", full.names = TRUE)
        cat("\033[31m", bibs, "\033[0m\n", ref)
      } else {
        stop("Wrong length")
      }
    } else {
      stop("Invalid input")
    }
  }

  for (x in names(self)) {
    if (typeof(self[[x]]) == "closure") {
      self[[x]] <- compiler::cmpfun(self[[x]], options = list(optimize = 3))
    }
  }
# output the enclosing environment ---------------------------------------------
  return(self)
})()




# Archive ----------------------------------------------------------------------
# writeRMD()
#
# writeRMD <- \() {
#   man <- list.files("man", full.names = TRUE)
#   hFun  <- c("<br>  \n", "| **Functions** | |", "| --- | :--- |")
#   hData <- c("<br>  \n", "| **Datasets**  | |", "| --- | :--- |")
#   flist <- list(
#     "function" = file("tools/function.Rmd", "w"),
#     "data" = file("tools/data.Rmd", "w")
#   )
#   cat(hFun, file = flist[["function"]], sep = "\n")
#   cat(hData, file = flist[["data"]], sep = "\n")
#
#   for (RdPath in man) {
#     x <- tools::parse_Rd(RdPath) |> extractNameTitle()
#     if (x$type == "package") next;
#     cat(x$val, file = flist[[x$type]], sep = "\n", append = TRUE)
#   }
#   for (x in flist) close(x)
# }
#


# extractNameTitle <- \(Rd) {
#   y = list(docType = NULL, name = NULL, title = NULL)
#   for (x in Rd) {
#     tag <- attr(x, "Rd_tag") |> gsub("\\\\", "", x = _)
#     if (utils::hasName(y, tag)) {
#       y[[tag]] <- unlist(x, use.names = FALSE) |> paste0(collapse = " ")
#       if (tag == "title") break;
#     }
#   }
#   if (is.null(y$docType)) y$docType <- "function"
#   y$title <- gsub("\n", "", y$title)
#   list(type = y$docType, val = sprintf("| [%s] | - %s |", y$name, y$title))
# }


#
# fmtExports <- \() {
#   self$exports <- extractTags() |>
#     (\(x) x[vapply(x, \(y) y$docType != "package", T)])() |>
#     lapply(\(x) {
#       sprintf("| [%s] | %s |", x$name, x$title) |>
#         data.frame(type = x$docType, val = _)
#     }) |> do.call(rbind, args = _)  |>
#     (\(x) split(x$val, x$type))() |>
#     lapply(paste0, collapse = "\n")
# }


# modified from http://r-pkgs.had.co.nz/man.html
# tabular <- \(df, ...) {
#   stopifnot(is.data.frame(df))
#   align <- \(x) if (is.numeric(x)) "r" else "l"
#   col_align <- vapply(df, align, character(1))
#   cols <- lapply(df, format, ...)
#   argContents <- c(cols, list(sep = " \\tab ", collapse = "\\cr\n"))
#   contents <- do.call("paste", argContents)
#   paste(
#     "\\tabular{", paste(col_align, collapse = ""), "}{\n",
#     contents, "\n}\n",
#     sep = ""
#   )
# }
#
# get_table_metadata <- \(csvDatName) {
#   path <- paste0("data-raw/", csvDatName, ".csv")
#   dt <- utils::read.csv(path, stringsAsFactors = FALSE)
#   paste0(readLines(textConnection(tabular(dt))))
# }
#
# genDataDoc <- \(dat) {
#   nams <- names(dat)
#   contents <- lapply(seq_along(nams), \(i) {
#     nn <- nams[[i]]
#     c(paste0("\\[,", i,  "\\]"),
#       nn,
#       paste0("\\code{", typeof(dat[[nn]]), "}"),
#       attr(dat[[nn]], "Description")
#     ) |> paste(... = _, collapse = " \\tab ")
#   }) |>
#     paste(... = _, collapse = "\\cr\n")
#   paste("\\tabular{llll}{\n", contents, "\n}\n", sep = "")
# }

# extractTags <- \(arguments = FALSE) {
#   list.files("man", full.names = TRUE) |>
#     lapply(tools::parse_Rd) |>
#     lapply(extractTagsUnit, arguments)
# }





