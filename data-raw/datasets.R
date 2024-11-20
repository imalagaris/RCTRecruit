load_all()
grips <- read.csv("data-raw/GRIPS_log_by_day_with_gaps.csv")
grips$N.Rows <- NULL
names(grips) <- c("ScreenDt", "Screened", "MetCriteria", "Enrolled")
grips <- grips[grips$Screened > 0, ]
grips$ScreenDt <- as.Date(grips$ScreenDt, format = "%m/%d/%Y")
grips <- grips[c("ScreenDt", "Enrolled")]

library(lubridate)
minYR1 = minYR2 <- min(grips$ScreenDt)
year(minYR2) = year(minYR2) + 1
maxYR1 = maxYR2 <- minYR2 - 1
year(maxYR2) = year(maxYR2) + 1

gripsYR1 <- grips[grips$ScreenDt <= maxYR1, ]
rownames(gripsYR1) <- NULL

gripsYR2 <- grips[with(grips, ScreenDt >= minYR2 & ScreenDt <= maxYR2), ]
rownames(gripsYR2) <- NULL

gripsWeeklyYR2 <- with(gripsYR2, days2weeks(ScreenDt, Enrolled))

gripsYR1$ScreenDt <- as.character(gripsYR1$ScreenDt)
gripsYR2$ScreenDt <- as.character(gripsYR2$ScreenDt)

desc <- list(
  ScreenDt = "Calendar date of the screening process for recruitment in the study",
  Enrolled = "Number of new subjects enrolled in the study on that date",
  week = "Calendar week",
  year = "Calendar year",
  enrolled = "Number of people enrolled that week",
  activeDays = "Number of days in that week when recruitment was active"
)

for (x in names(gripsYR1)) {
  attr(gripsYR1[[x]], "Description") <- desc[[x]]
  attr(gripsYR2[[x]], "Description") <- desc[[x]]
}

for (x in names(gripsWeeklyYR2)) {
  attr(gripsWeeklyYR2[[x]], "Description") <- desc[[x]]
}

usethis::use_data(gripsYR1, gripsYR2, gripsWeeklyYR2, overwrite = TRUE)
