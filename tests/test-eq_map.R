library(testthat)

test_that("eq_map runs without error", {
  map <-load_data() %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(date) >= 2000) %>%
    eq_map(annot_col = "date")

  expect_is(map, "leaflet")
  expect_is(map, "htmlwidget")
})
