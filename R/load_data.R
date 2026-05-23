#' Loads in original measles data sets
#'
#' @return measles data sets
#'
#' @importFrom readr read_csv
#'
#' @export

load_data <- function(){

  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv')
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv')

}

