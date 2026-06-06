#' Creates a horizontal bar plot comparing measles and Covid-19 cases by country
#'
#' @param year a single year
#'
#' @return a horizontal bar plot with two bars for each country
#'
#' @importFrom ggplot2 ggplot aes geom_col position_dodge scale_x_log10 scale_fill_manual labs theme_minimal theme element_blank element_text
#' @importFrom glue glue
#' @importFrom scales label_number cut_short_scale
#'
#' @export
measles_covid_barplot <- function(year) {
  if(!is.numeric(year)) {
    stop("Please enter a valid year.")
  }

  measles_covid_compare(year) |>
    ggplot(aes(y = country, x = cases, fill = disease)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    scale_x_log10(labels = label_number(scale_cut = cut_short_scale()),
                  breaks = 10^(0:8)) +
    scale_fill_manual(values = c("COVID-19" = "#D85A30",
                                 "Measles" = "#1D9E75")) +
    labs(title = glue("Measles vs. COVID-19 Cases in {year}"),
         subtitle = "Top 10 countries by COVID-19 case count — log scale",
         x = "Total Cases (log scale)", y = NULL, fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 10))
}










