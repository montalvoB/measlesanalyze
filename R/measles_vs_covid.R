#' Creates a table comparing measles and Covid-19 cases in two years
#'
#' @param year1 first comparison year
#' @param year2 second comparison year
#'
#' @return a formatted table comparing number of cases for two years
#'
#' @importFrom dplyr rename inner_join
#' @importFrom tidyr pivot_wider
#' @importFrom glue glue
#' @importFrom gt gt fmt_integer tab_spanner tab_header cols_label tab_style cell_text cells_column_labels
#'
#' @export
measles_vs_covid <- function(year1, year2) {
  if(!is.numeric(year1)) {
    stop("Please enter a valid year.")
  }
  if(!is.numeric(year2)) {
    stop("Please enter a valid year.")
  }

  compare1 <- measles_covid_compare(year1) |>
    pivot_wider(names_from = disease,
                values_from = cases) |>
    rename(
      !!glue::glue("Measles_{year1}") := Measles,
      !!glue::glue("COVID-19_{year1}") := "COVID-19"
      )

  compare2 <- measles_covid_compare(year2) |>
    pivot_wider(names_from = disease,
                values_from = cases) |>
    rename(
      !!glue::glue("Measles_{year2}") := Measles,
      !!glue::glue("COVID-19_{year2}") := "COVID-19"
    )

  inner_join(compare1, compare2, by = "country") |>
    gt() |>
    fmt_integer(columns = c(glue("Measles_{year1}"), glue("COVID-19_{year1}"),
                            glue("Measles_{year2}"), glue("COVID-19_{year2}"))) |>
    tab_spanner(
      label = glue("{year1}"),
      columns = c(glue("Measles_{year1}"), glue("COVID-19_{year1}"))
    ) |>
    tab_spanner(
      label = glue("{year2}"),
      columns = c(glue("Measles_{year2}"), glue("COVID-19_{year2}"))
    ) |>
    tab_header(
      title = "Total Measles and COVID-19 Cases by Country",
      subtitle = glue("Countries with Highest Concentration of COVID-19 Cases in {year1} and {year2}")
    ) |>
    cols_label(
      country = "Country",
      !!glue::glue("Measles_{year1}") := "Measles",
      !!glue::glue("Measles_{year2}") := "Measles",
      !!glue::glue("COVID-19_{year1}") := "COVID-19",
      !!glue::glue("COVID-19_{year2}") := "COVID-19"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    )
}


#' Helper function to compare measles and Covid-19 cases in a select year
#'
#' @param year1 a single year
#'
#' @return a wrangled data set
#'
#' @importFrom dplyr filter select starts_with rowwise mutate ungroup arrange desc inner_join slice_head case_when
#' @importFrom glue glue
#' @importFrom forcats fct_reorder
#' @importFrom tidyr pivot_longer
#'
#' @export
measles_covid_compare <- function(year1) {
  if(!is.numeric(year1)) {
    stop("Please enter a valid year.")
  }

  measles_year <- load_data_year() |>
    filter(year == year1) |>
    select(country, measles_total_year = measles_total)

  covid_year <- load_data_covid() |>
    select(country, starts_with(glue("X{year1}"))) |>
    rowwise() |>
    mutate(covid_total_year = max(c_across(starts_with(glue("X{year1}"))),
                                  na.rm = TRUE)) |>
    ungroup() |>
    select(country, covid_total_year)

  comparison_year <- inner_join(measles_year, covid_year, by = "country") |>
    arrange(desc(covid_total_year)) |>
    slice_head(n = 10) |>
    pivot_longer(cols = c(measles_total_year, covid_total_year),
                 names_to = "disease", values_to = "cases") |>
    mutate(disease = case_when(disease == glue("measles_total_year")
                               ~ "Measles",
                               disease == glue("covid_total_year")
                               ~ "COVID-19"),
           country = fct_reorder(country, cases, .fun = max))

  return(comparison_year)
}











