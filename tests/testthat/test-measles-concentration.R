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
