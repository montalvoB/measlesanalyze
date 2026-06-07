# measles vs covid
test_that("measles_vs_covid returns a gt object on valid input", {
  result <- measles_vs_covid(2020, 2021)
  expect_s3_class(result, "gt_tbl")
})

test_that("measles_vs_covid works at boundary year combinations", {
  expect_s3_class(measles_vs_covid(2020, 2023), "gt_tbl")  # min and max
  expect_s3_class(measles_vs_covid(2022, 2023), "gt_tbl")  # adjacent years
})

test_that("measles_vs_covid errors on non-numeric input", {
  expect_error(measles_vs_covid("2020", 2021), "Please enter a valid year.")
  expect_error(measles_vs_covid(2020, "2021"), "Please enter a valid year.")
  expect_error(measles_vs_covid(NULL, 2021),   "Please enter a valid year.")
  expect_error(measles_vs_covid(2020, NULL),   "Please enter a valid year.")
})


test_that("measles_vs_covid errors on out-of-range year", {
  expect_error(measles_vs_covid(2019, 2021), "`year1` must be between 2020 and 2023.")
  expect_error(measles_vs_covid(2020, 2024), "`year2` must be between 2020 and 2023.")
})

test_that("measles_vs_covid errors when year1 == year2", {
  expect_error(measles_vs_covid(2021, 2021), "`year1` and `year2` must be different years.")
})

# measles_covid_compare
test_that("measles_covid_compare returns a data frame on valid input", {
  result <- measles_covid_compare(2021)
  expect_s3_class(result, "data.frame")
})

test_that("measles_covid_compare has expected columns", {
  result <- measles_covid_compare(2021)
  expect_equal(names(result), c("country", "disease", "cases"))
})

test_that("measles_covid_compare returns exactly 20 rows (10 countries x 2 diseases)", {
  result <- measles_covid_compare(2021)
  expect_equal(nrow(result), 20)
})

test_that("measles_covid_compare disease values are only Measles and COVID-19", {
  result <- measles_covid_compare(2021)
  expect_setequal(unique(result$disease), c("Measles", "COVID-19"))
})

test_that("measles_covid_compare works at boundary years", {
  expect_s3_class(measles_covid_compare(2020), "data.frame")  # lower bound
  expect_s3_class(measles_covid_compare(2023), "data.frame")  # upper bound
})

test_that("measles_covid_compare errors on non-numeric input", {
  expect_error(measles_covid_compare("2021"), "Please enter a valid year.")
  expect_error(measles_covid_compare(NULL),   "Please enter a valid year.")
})

test_that("measles_covid_compare errors on out-of-range year", {
  expect_error(measles_covid_compare(2019), "`year_in`must be between 2020 and 2023.")
  expect_error(measles_covid_compare(2024), "`year_in`must be between 2020 and 2023.")
})
