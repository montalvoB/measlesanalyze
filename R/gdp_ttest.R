#' Creates a table showing the results of a two sample t-test on measles and GDP growth
#'
#' @param year_in a single year
#'
#' @return a table with 10 columns and one row of test results
#'
#' @importFrom dplyr inner_join join_by mutate filter
#' @importFrom glue glue
#' @importFrom gt gt tab_header md cols_label fmt_number
#'
#' @export
gdp_ttest <-function(year_in) {
  if(!is.numeric(year_in)) {
    stop("Please enter a valid year.")
  }
  if (year_in < 2012 || year_in > 2025) {
    stop("`year_in`must be between 2012 and 2025.")
  }
  country_gdp <- load_data_gdp()
  cases_year <- load_data_year()

  cases_gdp_join <- inner_join(cases_year, country_gdp,
                               by = join_by(year, iso3 == country_code)) |>
    mutate(
      high_measles = measles_lab_confirmed > median(measles_lab_confirmed)
    ) |>
    filter(year == year_in)

  tt <- t.test(gdp_growth ~ high_measles, data = cases_gdp_join)

  broom::tidy(tt) |>
    mutate(alternative = "Two-sided test (μ₁ ≠ μ₂)") |>
    gt() |>
    tab_header(
      title = md("**Two-Sample t-test Results**"),
      subtitle = glue("Comparison of mean GDP growth between high and low measles burden countries in {year_in}")
    ) |>
    cols_label(
      estimate1 = "Mean GDP growth (Low Measles)",
      estimate2 = "Mean GDP growth (High Measles)",
      statistic = "t statistic",
      parameter = "df",
      p.value = "p-value",
      estimate = "Mean difference",
      conf.low = "CI lower",
      conf.high = "CI upper",
      method = "Method",
      alternative = "Alternative Hypothesis"
    ) |>
    fmt_number(columns = everything(), decimals = 4)
}













