#' Loads in original monthly measles data set
#'
#' @return monthly measles data sets
#'
#' @importFrom readr read_csv
#'
#' @export
load_data_month <- function(){

  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_month.csv')

}

#' Loads in original yearly measles data set
#'
#' @return yearly measles data sets
#'
#' @importFrom readr read_csv
#'
#' @export
load_data_year <- function(){

  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-24/cases_year.csv')

}



