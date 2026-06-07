test_that("gdp_ttest returns a gt object on valid input", {
  result <- gdp_ttest(2019)
  expect_s3_class(result, "gt_tbl")
})

test_that("gdp_ttest works across boundary years", {
  expect_s3_class(gdp_ttest(2012), "gt_tbl")  # lower bound
  expect_s3_class(gdp_ttest(2025), "gt_tbl")  # upper bound
})

test_that("gdp_ttest errors on non-numeric input", {
  expect_error(gdp_ttest("2019"), "Please enter a valid year.")
  expect_error(gdp_ttest(NULL),   "Please enter a valid year.")
})

test_that("gdp_ttest errors on out-of-range year", {
  expect_error(gdp_ttest(2011), "`year_in`must be between 2012 and 2025.")
  expect_error(gdp_ttest(2026), "`year_in`must be between 2012 and 2025.")
})
