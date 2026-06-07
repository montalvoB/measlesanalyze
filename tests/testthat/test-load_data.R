test_that("load_data_year has expected shape", {
  dat <- load_data_year()
  expect_equal(nrow(dat), 2382)
  expect_equal(ncol(dat), 19)
  expect_true("country" %in% names(dat))
  expect_true("year" %in% names(dat))
  expect_true("measles_total" %in% names(dat))
})

test_that("load_data_month has expected shape", {
  dat <- load_data_month()
  expect_equal(nrow(dat), 22780)
  expect_equal(ncol(dat), 15)
  expect_true("country" %in% names(dat))
  expect_true("year" %in% names(dat))
  expect_true("measles_total" %in% names(dat))
})

test_that("load_data_year returns non-empty data", {
  dat <- load_data_year()
  expect_gt(nrow(dat), 0)
})

test_that("load_data_month returns non-empty data", {
  dat <- load_data_month()
  expect_gt(nrow(dat), 0)
})

test_that("load_data_year covers expected year range", {
  dat <- load_data_year()
  expect_true(all(dat$year >= 2012 & dat$year <= 2025))
})

test_that("load_data_month covers expected year range", {
  dat <- load_data_month()
  expect_true(all(dat$year >= 2012 & dat$year <= 2025))
})

test_that("load_data_year measles_total is numeric and non-negative", {
  dat <- load_data_year()
  expect_true(is.numeric(dat$measles_total))
  expect_true(all(dat$measles_total >= 0, na.rm = TRUE))
})
