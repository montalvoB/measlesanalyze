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
