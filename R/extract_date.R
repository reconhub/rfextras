#' Extract the date from a charcter string
#'
#' This function is used to extract a date from a `character` vector, using a
#' user-provided date parser. It first uses regular expressions to isolate
#' potential dates from the character strings, and then applies the parser (by
#' default, `lubridate::ymd`) to return a `Date` object. The function only
#' handles dates provided as numbers.
#'
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @export
#' 
#' @inheritParams find_latest
#'
#' @param x a `character` string containing dates to be extracted
#'
#' @examples
#'
#' x <- c("contacts_2017-10-29.xlsx",
#'        "some text! 2020 01 12",
#'        "2019.1.13 - linelist",
#'        "no date here :-/ ") 
#' extract_date(x)
#'

extract_date <- function(x, date_parser = lubridate::ymd) {
  date_pattern <- "[0-9]{1,4}[-_/ .]?[0-9]{1,2}[-_/ .]?[0-9]{1,4}"
  date_txt <- extract_string(x, date_pattern)
  date_parser(date_txt)
}
