library(reportfactory)
library(fs)


test_that("find_latest gives expected results", {

  skip_on_cran()

  ## create new factory and toy files
  odir <- getwd()
  f <- new_factory(path = path_temp())
  ## on.exit(dir_delete(f))
  ## on.exit(setwd(odir))

  file.create(file.path("data", "linelist_2020-10-01.xlsx"))
  file.create(file.path("data", "linelist_2020-10-12.csv"))
  file.create(file.path("data", "linelist.xlsx"))
  file.create(file.path("data", "contacts.xlsx"))
  file.create(file.path("data", "death_linelist_2020-10-13.xlsx"))
  cat("This is a test\n", file = file.path("data", "notes_2020-01-01.txt"))

  ## test a few paths
  expect_identical("/data/death_linelist_2020-10-13.xlsx",
                   sub(getwd(), "",
                       find_latest("linelist", quiet =  TRUE)))
  expect_identical("/data/linelist_2020-10-12.csv",
                   sub(getwd(), "",
                       find_latest("^linelist", quiet =  TRUE)))
  expect_identical(NULL,
                  find_latest("yakossomak", quiet = TRUE))

  ## test that we can actually read the file
  expected <- "This is a test"
  actual <- readLines(find_latest("notes", quiet = TRUE))
  expect_identical(expected, actual)


  ## because on.exit() is not working...
  dir_delete(f)
  setwd(odir)

})





test_that("find_latest gives expected messages, warnings and errors", {

  skip_on_cran()

  ## create new factory and toy files
  odir <- getwd()
  f <- new_factory(path = path_temp())
  ## on.exit(dir_delete(f))
  ## on.exit(setwd(odir))

  file.create(file.path("data", "linelist_2020-10-01.xlsx"))
  file.create(file.path("data", "linelist_2020-10-12.csv"))
  file.create(file.path("data", "linelist.xlsx"))
  file.create(file.path("data", "contacts.xlsx"))
  file.create(file.path("data", "death_linelist_2020-10-13.xlsx"))
  cat("This is a test\n", file = file.path("data", "notes_2020-01-01.txt"))

  ## test a few messages / errors
  ### when there are some NAs
  msg <- "lubridate::ymd could not find dates in files matching linelist"
  expect_message(find_latest("linelist", quiet =  FALSE),
                 msg)

  ### with a different date parser
  foobar <- lubridate::ymd
  msg <- "foobar could not find dates in files matching linelist"
  expect_message(find_latest("linelist",
                             date_converter = foobar,
                             quiet =  FALSE),
                 msg)

  ### no file matching
  msg <- sprintf("No file matching pattern 'yakossomak' found in '%s'",
                 getwd())
  expect_message(find_latest("yakossomak", quiet = FALSE),
                 msg)

  ## because on.exit() is not working...
  dir_delete(f)
  setwd(odir)

})




