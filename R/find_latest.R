#' Find latest version of a file
#'
#' If some file names include dates, this function will find the latest version
#' of a given file. This can be handy e.g. for finding the latest data update
#' automatically. `find_latest` scans recursively a directory, looking for file
#' names matching a given pattern (or regular expression) and including
#' dates. Dates are extracted using [extract_date](extract_date), using a
#' user-provided parser (see details). The full path to the most recent file
#' will be returned.
#'
#' @author Thibaut Jombart
#'
#' @export
#' 
#' @param pattern a character string indicating the name of the file to look for, or
#'   regular expression to be matched against file names
#'
#' @param where the directory in which to look for the file; defaults to the
#'     current directory
#'
#' @param date_parser a function to extract dates from character strings; we
#'   recommend using `lubridate`'s function there, e.g. `ymd` (any date format
#'   with year, month, day) or `dmy` (any format using day, month, year);
#'   defaults to `lubridate::ymd`
#'
#' @param quiet a `logical` indicating if messages should be displayed (`TRUE`,
#'   default), or not (`FALSE`)
#'
#' @param ... further arguments passed to [`list.files`](list.files)
#'
#' @details The date parser used by default is [lubridate::ymd](lubridate::ymd),
#'     so that any date specified as year/month/day (regardless of the separator
#'     used) should work. One exception is that only dates provided as numbers
#'     can be used e.g. "1982.02.04" and "82/2/4" are okay but not
#'     "1982 Feb 04". For dates provided in a different order, check other
#'     parsers implemented in `lubridate`, e.g. `lubridate::dmy` for
#'     day-month-year formats, or `lubridate::ydm` for year-day-month. Note that
#'     parsers for date-time objects can also be used, but in any case a `Date`
#'     object will be returned.
#'
#' @seealso [extract_date](extract_date) to extract dates from `character`
#'     strings; see the `lubridate` [website](https://lubridate.tidyverse.org/)
#'     and
#'     [cheatsheet](https://rawgit.com/rstudio/cheatsheets/master/lubridate.pdf)
#'     for other parsers
#'
#' @examples  
#'
#' if (require(reportfactory)) {
#' 
#' ## create random factory and toy files
#' odir <- getwd()
#' f <- new_factory(path = tempdir())
#' 
#' file.create(file.path("data", "linelist_2020-10-01.xlsx"))
#' file.create(file.path("data", "linelist_2020-10-12.csv"))
#' file.create(file.path("data", "linelist.xlsx"))
#' file.create(file.path("data", "contacts.xlsx"))
#' file.create(file.path("data", "death_linelist_2020-10-13.xlsx"))
#'
#' ## find the latest data with 'linelist' in the name; note that this
#' ## matches both 'linelist' and 'death_linelist' files
#' find_latest("linelist")
#'
#' ## same, but this time files starting with 'linelist', i.e. excluding
#' ## 'death_linelist'
#' find_latest("^linelist")
#'
#' ## this returns NULL
#' find_latest("foobar")
#'
#' ## cleanup
#' unlink(f, recursive = TRUE)
#' setwd(odir)
#' }

find_latest <- function(pattern,
                        where = getwd(),
                        date_parser = lubridate::ymd,
                        quiet = FALSE,
                        ...) {

  if (!inherits(date_parser, "function")) {
    msg <- "`date_parser` is not a function"
    stop(msg)
  }

  # Approach
  #
  # 1. check the directory provided as 'where' and make sure it exists
  #
  # 2. look recursively for 'pattern' in this directory; issue message if there
  # are no matches
  #
  # 3. extract dates for all returned files; issue message if dates are missing
  #
  # 4. return the full path to the latest file


  # step 1
  if (!dir.exists(where)) {
    msg <- sprintf("directory '%s' does not exist",
                   where)
    stop(msg)
  }


  # step 2
  all_files <- list.files(where,
                          pattern = pattern,
                          full.names = TRUE,
                          recursive = TRUE,
                          ...)

  if (!length(all_files)) {
    msg <- sprintf("No file matching pattern '%s' found in '%s'",
                   pattern,
                   where)
    if (!quiet) message(msg)
    return(NULL)
  }


  # step 3: get dates; note that there could be multiple dates in the full path
  # to a file, and only the one in the basename is used
  base_files <- basename(all_files)
  file_dates <- extract_date(base_files, date_parser)

  if (any(is.na(file_dates)) && !quiet) {
    msg <- sprintf(
      "%s could not find dates in files matching %s",
      deparse(substitute(date_parser)),
      pattern)
    message(msg)
  }

  to_keep <- !is.na(file_dates)
  base_files <- base_files[to_keep]
  file_dates <- file_dates[to_keep]
  all_files <- all_files[to_keep]
  last_file_index <- which.max(file_dates)
  out <- all_files[last_file_index]


  # step 4
  if (!quiet) {
    msg <- sprintf("Found file: '%s'",
                   base_files[last_file_index])
    message(msg)
  }
  out

}
