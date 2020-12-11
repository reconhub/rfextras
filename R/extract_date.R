
#' Extract the date from a charcter string (vectorised)
#'
#' This function is used to extract a date from a string
#'
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#'
#' @inheritParams find_latest
#'
#' @param x a `character` string containing dates to be extracted
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' x <- c("contacts_2017-10-29.xlsx",
#'        "some text! 2020 01 12",
#'        "2019.1.13 - linelist"
#' extract_date(x)
#'
#' }
#'

extract_date <- function(x, date_converter = lubridate::ymd) {
  date_pattern <- "[0-9]{1,4}[-_/ .]?[0-9]{1,2}[-_/ .]?[0-9]{1,4}"
  date_txt <- extract_string(x, date_pattern)
  date_converter(date_txt)
}
