#' Loads in original monthly measles data set
#'
#' @return monthly measles data set
#'
#' @importFrom utils read.csv
#'
#' @export
load_data_month <- function(){

  path <- system.file("extdata", "cases_month.csv", package = "measlesanalyze")
  read.csv(path)

}

#' Loads in original yearly measles data set
#'
#' @return yearly measles data set
#'
#' @importFrom utils read.csv
#'
#' @export
load_data_year <- function(){

  path <- system.file("extdata", "cases_year.csv", package = "measlesanalyze")
  read.csv(path)

}

#' Loads in cleaned Covid-19 data set
#'
#' @return Covid-19 data set
#'
#' @importFrom utils read.csv
#'
#' @export
load_data_covid <- function(){

  path <- system.file("extdata", "covid_cleaned.csv", package = "measlesanalyze")
  read.csv(path)

}



