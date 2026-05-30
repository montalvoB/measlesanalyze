#' Creates a table a table showing the concentration of measles cases
#'
#' @param start_year starting year for analysis
#' @param end_year ending year for analysis
#'
#' @return a formatted table with 6 columns of information
#'
#' @importFrom dplyr filter distinct pull between
#' @importFrom purrr map_dfr
#' @importFrom glue glue
#' @importFrom gt gt tab_header tab_spanner cols_label fmt_percent fmt_number tab_style cells_column_labels cell_text
#'
#' @export
measles_concentration <- function(start_year, end_year) {
  key_years <- load_data_year() |>
    filter(between(year, start_year, end_year)) |>
    distinct(year) |>
    pull(year)

  concentration_over_time <- map_dfr(key_years, summarize_concentration)

  concentration_over_time |>
    gt() |>
    tab_header(
      title = "Global Measles Concentration by Year",
      subtitle = glue("How burden is distributed across countries, {start_year}–{end_year}")
    ) |>
    cols_label(
      year = "Year",
      global_total = "Global Cases",
      top1_country = "Top Country",
      top1_share = "Top Country",
      top2_3_share = "2nd & 3rd Countries",
      n_major = "# Major Burden Countries"
    ) |>
    tab_spanner(
      label = "Share of Global Cases",
      columns = c(top1_share, top2_3_share)
    ) |>
    fmt_percent(
      columns = c(top1_share, top2_3_share),
      decimals = 1
    ) |>
    fmt_number(columns = global_total, use_seps = TRUE, decimals = 0) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    )
}


#' Helper function to calculate concentration
#'
#' @param year_in the year in which concentration is calculated
#'
#' @return a wrangled data set
#'
#' @importFrom dplyr filter mutate select arrange desc case_when
#'
#' @export
calc_concentration <- function(year_in) {
  load_data_year() |>
    filter(year == year_in) |>
    mutate(
      global_total = sum(measles_total, na.rm = TRUE),
      share = measles_total / global_total,
      burden_tier = case_when(
        share >= 0.10 ~ "Major (10%+ of global)",
        share >= 0.01 ~ "Significant (1-10%)",
        share >= 0.001 ~ "Minor (0.1-1%)",
        TRUE ~ "Negligible"
      )
    ) |>
    filter(burden_tier != "Negligible") |>
    select(country, measles_total, share, burden_tier) |>
    arrange(desc(share))
}


#' Helper function to summarize concentration
#'
#' @param yr a single year
#'
#' @return a wrangled data set
#'
#' @importFrom dplyr summarize first
#'
#' @export
summarize_concentration <- function(yr) {
  calc_concentration(yr) |>
    summarize(
      year = yr,
      global_total = sum(measles_total),
      top1_country = first(country),
      top1_share = first(share),
      top2_3_share = sum(share[2:3]),
      n_major = sum(burden_tier == "Major (10%+ of global)")
    )
}








