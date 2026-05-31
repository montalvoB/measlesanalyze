test_that("plot_measles_heatmap works", {
  dat <- load_data_year()

  heatmap_simple    <- plot_measles_heatmap(2015, 2019)
  heatmap_one_year  <- plot_measles_heatmap(2019, 2019)
  heatmap_full_range <- plot_measles_heatmap(2012, 2024)

  expect_s3_class(heatmap_simple,     "ggplot")
  expect_s3_class(heatmap_one_year,   "ggplot")
  expect_s3_class(heatmap_full_range, "ggplot")
})

test_that("plot_measles_heatmap errors on non-numeric input", {
  expect_error(plot_measles_heatmap("2015", 2019), "Please enter a valid year.")
  expect_error(plot_measles_heatmap(2015, "2019"), "Please enter a valid year.")
  expect_error(plot_measles_heatmap(NULL,   2019), "Please enter a valid year.")
})
