source("data-raw/probs.R")
source("data-raw/dataDocHelpers.R")
usethis::use_data(probs, wts, e, overwrite = TRUE, internal = TRUE)
