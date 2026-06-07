test_that("measles_covid_barplot returns a ggplot on valid input", {
  result <- measles_covid_barplot(2021)
  expect_s3_class(result, "ggplot")
})

test_that("measles_covid_barplot works at boundary years", {
  expect_s3_class(measles_covid_barplot(2020), "ggplot")  # lower bound
  expect_s3_class(measles_covid_barplot(2023), "ggplot")  # upper bound
})

test_that("measles_covid_barplot errors on out-of-range year", {
  expect_error(measles_covid_barplot(2019), "`year` must be between 2020 and 2023.")
  expect_error(measles_covid_barplot(2024), "`year` must be between 2020 and 2023.")
})

test_that("measles_covid_barplot errors on non-numeric input", {
  expect_error(measles_covid_barplot("2021"), "Please enter a valid year.")
  expect_error(measles_covid_barplot(NULL),   "Please enter a valid year.")
})

test_that("measles_covid_barplot plot has expected aesthetics", {
  p <- measles_covid_barplot(2021)
  # x and y mappings are present
  expect_equal(rlang::as_name(p$mapping$x), "cases")
  expect_equal(rlang::as_name(p$mapping$y), "country")
  # fill mapping is present
  expect_equal(rlang::as_name(p$mapping$fill), "disease")
})
