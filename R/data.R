#' Daily recruitment data for the 1st year of the GRIPS study
#' `r dat <- gripsYR1;`
#'
#' Modified recruitment data from the GRIPS study for the first year the study
#' was conducted. The data includes the number of participants recruited each
#' for each calendar date the recruitment was active.
#' @rawRd % d1
#' @format
#' A data frame with
#'     **`r nrow(dat)`** observations of
#'     **`r length(dat)`** variables:
#' `r e$genDataDoc1(gripsYR1)`
#' @source `r e$addRef("Villasante2024")`
"gripsYR1"

#' Daily recruitment data for the 2nd year of the GRIPS study
#' `r dat <- gripsYR2;`
#'
#' Modified recruitment data from the GRIPS study for the second year the study
#' was conducted. The data includes the number of participants recruited each
#' for each calendar date the recruitment was active.
#' @rawRd % d2
#' @format
#' A data frame with
#'     **`r nrow(dat)`** observations of
#'     **`r length(dat)`** variables:
#' `r e$genDataDoc1(gripsYR2)`
#' @source `r e$addRef("Villasante2024")`
"gripsYR2"

#' Weekly recruitment data for the 2nd year of the GRIPS study
#' `r dat <- gripsWeeklyYR2;`
#'
#' Modified recruitment data aggregated by week from the second year of the
#' GRIPS study.
#' @rawRd % d3
#' @format
#' A data frame with
#'     **`r nrow(dat)`** observations of
#'     **`r length(dat)`** variables:
#' `r e$genDataDoc1(gripsWeeklyYR2)`
#' @source `r e$addRef("Villasante2024")`
"gripsWeeklyYR2"
