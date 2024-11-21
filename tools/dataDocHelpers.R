e <- (\() {
  citationPath <- "tools/citations/"

  # modified from http://r-pkgs.had.co.nz/man.html
  tabular <- \(df, ...) {
    stopifnot(is.data.frame(df))
    align <- \(x) if (is.numeric(x)) "r" else "l"
    col_align <- vapply(df, align, character(1))
    cols <- lapply(df, format, ...)
    argContents <- c(cols, list(sep = " \\tab ", collapse = "\\cr\n"))
    contents <- do.call("paste", argContents)
    paste(
      "\\tabular{", paste(col_align, collapse = ""), "}{\n",
      contents, "\n}\n",
      sep = ""
    )
  }

  get_table_metadata <- \(csvDatName) {
    path <- paste0("data-raw/", csvDatName, ".csv")
    dt <- utils::read.csv(path, stringsAsFactors = FALSE)
    paste0(readLines(textConnection(tabular(dt))))
  }

  genDataDoc <- \(dat) {
    nams <- names(dat)
    contents <- lapply(seq_along(nams), \(i) {
      nn <- nams[[i]]
      c(paste0("\\[,", i,  "\\]"),
        nn,
        paste0("\\code{", typeof(dat[[nn]]), "}"),
        attr(dat[[nn]], "Description")
      ) |> paste(... = _, collapse = " \\tab ")
    }) |>
      paste(... = _, collapse = "\\cr\n")
    paste("\\tabular{llll}{\n", contents, "\n}\n", sep = "")
  }

  bibname <- \(x) paste0(citationPath, x, ".bib")

  addRef <- \(fname) {
    bibname(fname) |>
      bibtex::read.bib() |>
      utils:::format.bibentry()
  }

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

  readDesc <- \() {
    read.dcf("DESCRIPTION") |>
      as.data.frame() |>
      lapply(\(x) gsub("\\n", "", x))
  }

  getExportedFunctions <- \() {
    reg <- "^export\\((\\w+)\\)$"
    readLines("NAMESPACE") |>
      grep(reg, x = _, value = TRUE) |>
      gsub(reg, "\\1", x = _)
  }

  docExportedFunctions <- \() {
    getExportedFunctions() |> paste0("[", ... = _, "]", collapse = ", ")
  }

  getRdTitle <- \(RdFile) {
    RdContent <- tools::parse_Rd(RdFile)
    for (item in RdContent) {
      if (attr(item, "Rd_tag") == "\\title") {
        out <- paste0(item, collapse = "") |> gsub("\n", " ", x = _)
        return(out)
      }
    }
  }

  getExportedItems <- \() {
    out <- list()
    man <-
      list.files("man", full.names = TRUE) |>
      grep("(?<!-package)\\.Rd$", x = _, perl = TRUE, value = TRUE)
    for (Rd in man) {
      nn <- gsub("^man/(\\w+)\\.Rd", "\\1", Rd)
      out[[nn]] <- list(type = "data", name = nn, desc = getRdTitle(Rd))
    }
    for (fn in getExportedFunctions()) {
      out[[fn]]$type <- "function"
    }
    names(out) <- NULL
    do.call(rbind, out) |> as.data.frame()
  }

  return(environment())
})()
