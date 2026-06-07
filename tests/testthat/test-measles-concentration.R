test_that("calc_concentration works", {
  dat <- load_data_year()

  result <- calc_concentration(2019)

  # Check object type
  expect_s3_class(result, "data.frame")

  # Check expected column names
  expect_equal(names(result), c("country", "measles_total", "share", "burden_tier"))

  # Check sorted descending by share
  expect_true(all(diff(result$share) <= 0))

  # Check Negligible tier is filtered out
  expect_false("Negligible" %in% result$burden_tier)

  # Check share values are valid proportions
  expect_true(all(result$share >= 0 & result$share <= 1))

  # Check expected top country
  expect_equal(result$country[1], "Madagascar")
})

test_that("summarize_concentration works", {
  dat <- load_data_year()

  result <- summarize_concentration(2019)

  # Check object type
  expect_s3_class(result, "data.frame")

  # Check expected column names
  expect_equal(names(result), c("year", "global_total", "top1_country",
                                "top1_share", "top2_3_share", "n_major"))

  # Check expected values
  expect_equal(result$year, 2019)
  expect_equal(result$global_total, 532303)
  expect_equal(result$top1_country, "Madagascar")
  expect_equal(result$top1_share, 0.393961, tolerance = 1e-4)
  expect_equal(result$top2_3_share, 0.195247, tolerance = 1e-4)
  expect_equal(result$n_major, 2)
})

test_that("measles_concentration returns a gt object", {
  dat <- load_data_year()

  result <- measles_concentration(2019, 2019)

  expect_s3_class(result, "gt_tbl")
})

test_that("calc_concentration errors on non-numeric input", {
  expect_error(calc_concentration("2019"), "Please enter a valid year.")
  expect_error(calc_concentration(NULL), "Please enter a valid year.")
})

test_that("summarize_concentration errors on non-numeric input", {
  expect_error(summarize_concentration("2019"), "Please enter a valid year.")
  expect_error(summarize_concentration(NULL), "Please enter a valid year.")
})

test_that("measles_concentration errors on non-numeric input", {
  expect_error(measles_concentration("2019", 2020), "Please enter a valid year.")
  expect_error(measles_concentration(2019, "2020"), "Please enter a valid year.")
})

test_that("measles_concentration works with default arguments", {
  result <- measles_concentration()
  expect_s3_class(result, "gt_tbl")
})

test_that("measles_concentration errors when start_year is out of range", {
  expect_error(measles_concentration(2011, 2020), "`start_year` must be greater than 2011.")
})

test_that("measles_concentration errors when end_year is out of range", {
  expect_error(measles_concentration(2012, 2026), "`end_year` must be less than 2026.")
})

test_that("measles_concentration errors when start_year is after end_year", {
  expect_error(measles_concentration(2020, 2015), "`start_year` must be less than `end_year`")
})

# calc_concentration
test_that("calc_concentration errors on out-of-range year", {
  expect_error(calc_concentration(2011), "`year_in`must be between 2012 and 2025.")
  expect_error(calc_concentration(2026), "`year_in`must be between 2012 and 2025.")
})

test_that("calc_concentration works at boundary years", {
  expect_s3_class(calc_concentration(2012), "data.frame")
  expect_s3_class(calc_concentration(2025), "data.frame")
})

test_that("calc_concentration share column sums to <= 1", {
  result <- calc_concentration(2019)
  # Negligible countries are filtered, so sum of shares won't hit 1, but must not exceed it
  expect_lte(sum(result$share), 1)
})

# summarize_concentration
test_that("summarize_concentration errors on out-of-range year", {
  expect_error(summarize_concentration(2011), "Enter a year from 2012 to 2025.")
  expect_error(summarize_concentration(2026), "Enter a year from 2012 to 2025.")
})

test_that("summarize_concentration works at boundary years", {
  expect_s3_class(summarize_concentration(2012), "data.frame")
  expect_s3_class(summarize_concentration(2025), "data.frame")
})

test_that("summarize_concentration top1_share is between 0 and 1", {
  result <- summarize_concentration(2019)
  expect_gte(result$top1_share, 0)
  expect_lte(result$top1_share, 1)
})
