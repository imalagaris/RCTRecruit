#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib RCTRecruit, .registration = TRUE
#'
## usethis namespace: end
NULL
#' @name RCTRecruit-package
#' @details
#' | Package: | RCTRecruit                                        |
#' | -------  | :---------                                        |
#' | Type:    | Package                                           |
#' | Version: | `r read.dcf("DESCRIPTION")[1L, "Version"][[1L]]`  |
#' | Date:    | `r Sys.Date()`                                    |
#' | License: | `r read.dcf("DESCRIPTION")[1L, "License"][[1L]]`  |
#' | Exported functions: | `r e$getExportedFunctions()`  |
#' @references
#' 1. `r e$addRef("Villasante2024")`
#' 1. `r e$addRef("Gajewski2008")`
#' 1. `r e$addRef("Jiang2015")`
#' @seealso `r e$addRef("accrual-package")`
#' @md
NULL
