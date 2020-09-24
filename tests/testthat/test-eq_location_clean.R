library(noarr)
library(testthat)

test_that("Location is cleaned properly", {
  df <- load_data() %>% eq_location_clean
  expect_true(all(df$LOCATION_NAME[1:2] == c("JORDAN:  BAB-A-DARAA,AL-KARAK", "SYRIA:  UGARIT")))
  expect_true(all(df$CLEAN_LOCATION_NAME[1:2] == c("Bab-A-Daraa,Al-Karak", "Ugarit")))
})

