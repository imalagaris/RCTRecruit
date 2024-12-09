test_that("Load Success", {
  LoadData(gripsYR1, ScreenDt, Enrolled) |> expect_no_error()
  LoadData(data = gripsYR1, ScreenDt, Enrolled) |> expect_no_error()
  LoadData(gripsYR1, "ScreenDt", "Enrolled") |> expect_no_error()
  gripsYR1 |> LoadData("ScreenDt", "Enrolled") |> expect_no_error()
})

test_that("Load with magrittr pipe", {
  skip_if(!hasPack("magrittr"))
  withr::local_package("magrittr")
  gripsYR1 %>% LoadData("ScreenDt", "Enrolled") |> expect_no_error()
})

test_that("Load errors", {
  LoadData(gripsYR1, ScreenDt1, Enrolled) |> expect_error()
  LoadData("gripsYR1", ScreenDt, Enrolled) |> expect_error()
  LoadData(gripsYR1, ScreenDt, Enrolled1) |> expect_error()
  LoadData(NULL, ScreenDt, Enrolled) |> expect_error()
  LoadData(gripsYR1, NULL, Enrolled) |> expect_error()
  LoadData(gripsYR1, ScreenDt, NULL) |> expect_error()
  LoadData(gripsYR1, c(1, 5), Enrolled) |> expect_error()
})

test_that("Exported functions", {
  the$TrainVector <- NULL
  GetWeekPredCI(10L) |> expect_error()
  LoadData(gripsYR1, ScreenDt, Enrolled)
  nSimulations <- 10L
  Time2Nsubjects(nSimulations, 10L, coeff = 2) |> print() |> expect_no_error()
  Time2Nsubjects(nSim = nSimulations, 10L) |> print() |> expect_no_error()
  GetWeekPredCI(10L) |> print() |> expect_no_error()
  c(gripsYR2Weekly$enrolled, 0L) |>
    GetDistance(10L) |> 
    print() |> 
    expect_no_error()
  GetDistance(rep(1L, 5), 10L) |> expect_error()
  res <- GetWeekPredCI(10L)
  
  head(res$predCI) |> logPrint() |> expect_no_error()
  res$plot(yMax = 40, Title = "Hi") |> expect_no_error()
  res$pargs$reset() |> expect_no_error()
  res$pargs$addTarget(gripsYR2Weekly$enrolled) |> expect_no_error()
  gripsYR2Weekly$enrolled[[-1L]] |> GetDistance(10L) |>  expect_error()
  
})

test_that("date and enrolled", {
  fixDate(TRUE) |> expect_error()
  fixEnrolled(TRUE) |> expect_error()
  fixEnrolled(Inf) |> expect_error()
  fixEnrolled(Inf - Inf) |> expect_error()
  fixEnrolled(NA_integer_) |> expect_error()
  fixEnrolled(c(-1, 2)) |> expect_error()
  fixDate(rep(NA_real_, 11)) |> suppressWarnings() |> expect_error()
  the$color <- FALSE
  em("color") |> expect_no_error()
})
