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

test_that("plot_measles_heatmap errors on out-of-range years", {
  expect_error(plot_measles_heatmap(2011, 2019), "`start_year` must be greater than 2011.")
  expect_error(plot_measles_heatmap(2012, 2026), "`end_ear` must be less than 2026.")
})

test_that("plot_measles_heatmap works with default arguments", {
  result <- plot_measles_heatmap()
  expect_s3_class(result, "ggplot")
})

test_that("plot_measles_heatmap errors when start_year is after end_year", {
  expect_error(plot_measles_heatmap(2020, 2015), "`start_year` must be less than `end_year`")
})
