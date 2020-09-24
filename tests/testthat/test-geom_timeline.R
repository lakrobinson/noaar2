library(testthat)

test_that("geom_timeline runs correctly", {
  df <- load_data() %>% eq_clean_data() %>% dplyr::filter(COUNTRY %in% c("GREECE", "USA"), YEAR > 2002)
  plt <-   ggplot2::ggplot(df, ggplot2::aes(x = date, y = COUNTRY,
                                            color = as.numeric(TOTAL_DEATHS),
                                            size = as.numeric(EQ_PRIMARY),
                                            label = CLEAN_LOCATION_NAME)) +
    geom_timeline() +
    ggplot2::labs(size = "Richter scale value", color = "# deaths") +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank()) + ggplot2::xlab("DATE")

  expect_is(plt, "gg")
  expect_is(plt, "ggplot")
})
