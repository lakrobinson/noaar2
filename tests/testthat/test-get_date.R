#library(noarr) trying this
library(testthat)

test_that("The date column is generated properly", {
  years = c(1, 2017, 1985, 2001)
  months = c(12, 11, NA, NA)
  days = c(2, NA, 15, NA)

  dates <- get_date(days, months, years)
  expect_equal(length(dates), 3)             # expect a return of 3 dates
  expect_is(dates, "Date")                   # DATES should be the class "date"
  expect_equal(format(dates[4], "%m"), "01") # NA months replaced with "Jan"
  expect_equal(format(dates[4], "%d"), "01") # NA days replaced with "1st"
})
