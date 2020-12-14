

# basic test with a single date
test_that("extract_date gives expected results", {

  ## ymd formats
  x <- c("contacts_2017-10-29",
         "linelist 1998 01 03.xlsx",
         "some date 200123 and some stuff")
  expected <- as.Date(c("2017-10-29",
                      "1998-01-03",
                      "2020-01-23"))
  actual <- extract_date(x)
  expect_equal(expected, actual)

  ## dmy formats
  x <- c(" contacts_29/10/2017  ",
         "linelist 3.1.98.xlsx",
         "some date 23-01-20 and some stuff")
  actual <- extract_date(x, lubridate::dmy)
  expect_equal(expected, actual)

})





# Test messages, warnings and errors
test_that("extract_date gives expected messages/warnings/errors", {

  msg <- "`date_parser` is not a function"
  expect_error(extract_date("", "toto"),
               msg)
  
})
