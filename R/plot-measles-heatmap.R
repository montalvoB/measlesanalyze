#' Creates a heatmap plot of countries with the highest concentration of cases
#'
#' @param start_year starting year for analysis
#' @param end_year ending year for analysis
#'
#' @return a styled heatmap including 15 countries
#'
#' @importFrom dplyr filter between group_by summarize slice_max pull mutate ungroup
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient scale_x_continuous labs theme_minimal theme element_blank element_text
#' @importFrom forcats fct_reorder
#' @importFrom glue glue
#'
#' @export
plot_measles_heatmap <- function(start_year, end_year) {
  if(!is.numeric(start_year)) {
    stop("Please enter a valid year.")
  }
  if(!is.numeric(end_year)) {
    stop("Please enter a valid year.")
  }

  top_countries <- load_data_year() |>
    filter(between(year, start_year, end_year)) |>
    group_by(country) |>
    summarize(total = sum(measles_total, na.rm = TRUE)) |>
    slice_max(total, n = 15) |>
    pull(country)

  load_data_year() |>
    filter(between(year, start_year, end_year)) |>
    group_by(year) |>
    mutate(global_share = measles_total / sum(measles_total, na.rm = TRUE)) |>
    ungroup() |>
    filter(country %in% top_countries) |>
    ggplot(aes(x = year,
               y = fct_reorder(country, global_share),
               fill = global_share)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradient(low = "cornsilk", high = "tomato1",
                        labels = scales::percent,
                        name = "Share of\nGlobal Cases") +
    scale_x_continuous(breaks = start_year:end_year) +
    labs(
      title = "Measles Burden Is Dominated by a Handful of Countries",
      subtitle = glue("Share of global cases among top 15 burden countries, {start_year}–{end_year}"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}






