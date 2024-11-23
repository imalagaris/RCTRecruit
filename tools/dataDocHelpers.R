e <- (\() {
  self <- environment()

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
    varfmt <- "| \\[,%s\\] | %s | `%s` | %s |"
    dataHeader <- c(
      "  ",
      "|      |      |      |      |",
      "| :--- | :--- | :--- | :--- |"
    )
    l <- getDatasetInfo(df)
    size <- sprintf(dataSizeFmt, l$rows, l$cols )
    vars <- lapply(l$vars, \(x) do.call(sprintf, c(varfmt, x)))
    c(size, dataHeader, vars) |> unlist() |> paste0(collapse = "\n")
  }

# Rd Tags ----------------------------------------------------------------------
  readDesc <- read.dcf
  formals(readDesc) <- c(
    formals(readDesc)[2:3],
    file = "DESCRIPTION",
    keep.white = "Date"
    )
  fields <- c("Package", "Type", "Version", "Date", "License")
  getRd <- tools::parse_Rd
  fmtDetails <- \(nam, val) sprintf("| %s: | %s | |", nam, val)

  getRdNameTitle <- \(Rd) {
    y = list(docType = NULL, name = NULL, title = NULL)
    for (x in Rd) {
      tag <- attr(x, "Rd_tag") |> gsub("\\\\", "", x = _)
      if (utils::hasName(y, tag)) {
        y[[tag]] <- unlist(x, use.names = FALSE) |> paste0(collapse = " ")
        if (tag == "title") break;
      }
    }
    if (is.null(y$docType)) y$docType <- "func"
    y$title <- gsub("\n", "", y$title)
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
      x <- getRd(Rd) |> getRdNameTitle()
      if (x$type == "package") next;
      l[[x$type]][[x$name]] <- x$val
    }
    unlist(l, use.names = F) |> paste0(collapse = "\n")
  }

  getPackageDetails <- \() {
    header <- c(
      "|      |      |      |",
      "| :--- | :--- | :--- |"
    )
    pack <- readDesc(fields) |> as.data.frame()
    pack$Date <- Sys.Date()
    details <- vapply(names(pack), \(x) fmtDetails(x, pack[[x]]), "a")
    exported <- exportedTab()
    c(header, details, exported) |> paste0(collapse = "\n")
  }

# References -------------------------------------------------------------------
  citationPath <- "tools/citations/"
  bibname <- \(x) paste0(citationPath, x, ".bib")
  readBib <- \(x) bibtex::read.bib(x) |> utils:::format.bibentry()
  addRef <- \(x) bibname(x) |> readBib()

  mendeley2bib <- \(fname) {
    ref <- readClipboard()
    if (grepl("^@(article|Manual)", ref[[1L]])) {
      if (length(ref) > 4L) {
        write(ref, file = bibname(fname))
        bibs <- list.files(citationPath, "*\\.bib", full.names = TRUE)
        cat("\033[31m", bibs, "\033[0m\n", ref)
      } else {
        stop("Wrong length")
      }
    } else {
      stop("Invalid input")
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
#     x <- getRd(RdPath) |> extractNameTitle()
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


# extractTagsUnit <- \(Rd, arguments = FALSE) {
#   keep <- c("name", "docType", "title")
#   if (arguments) {
#     keep <- c(keep, "arguments")
#   }
#   item <- stats::setNames(vector("list", length(keep)), keep)
#
#   for (x in Rd) {
#     tag <- attr(x, "Rd_tag") |> gsub("\\\\", "", x = _)
#     if (utils::hasName(item, tag)) {
#       item[[tag]] <- unlist(x) |> paste0(collapse = " ")
#     }
#   }
#   if (is.null(item$docType)) {
#     item$docType <- "function"
#     if (arguments) {
#       item$arguments <- gsub("^\n | \n$", "", item$arguments)
#       item$arguments <-
#         strsplit(item$arguments, "\n \n ") |>
#         unlist() |>
#         gsub("\\s+$", "", x = _)
#     }
#   }
#   if (!is.null(item$description)) {
#     item$description <- gsub("^\n\\s*|\\s*\n$", "", item$description)
#   }
#   item
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
#     lapply(getRd) |>
#     lapply(extractTagsUnit, arguments)
# }













