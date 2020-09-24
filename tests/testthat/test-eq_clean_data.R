library(rcap)
context("Test clean_data function")

test_that("Data is cleaned properly", {
  df <- load_data() %>% eq_clean_data()
  expect_is(df, "data.frame")        # the result will be a dataframe
  expect_is(df$date, "Date")         # the DATE column class will be "date"
  expect_is(df$LATITUDE, "numeric")  # the LATITUDE column class will be "numeric"
  expect_is(df$LONGITUDE, "numeric") # the LONGITUDE column class will be "numeric"
})
