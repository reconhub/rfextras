library(reportfactory)
library(fs)

test_that("load_scripts works when files exist", {

  # create factory
  f <- new_factory(path = path_temp(), move_in = FALSE)
  on.exit(dir_delete(f))
  
  cat("a <- 1L", file = file.path(f, "scripts", "a.R"))
  cat("b <- 2L + a", file = file.path(f, "scripts", "b.R"))

  load_scripts(f, quiet = TRUE)
  expect_identical(b, 3L)
  
})





test_that("messages and errors as expected", {

  # create factory
  f <- new_factory(path = tempdir(), move_in = FALSE)
  on.exit(dir_delete(f))

  ## messages with no scripts / missing folders
  msg <- "No `.R` files in"
  expect_message(load_scripts(f), msg)

  msg <- "does not exist"
  expect_error(load_scripts(f, "bob"), "bob does not exist")
  
  ## messages when reading scripts
  cat("a <- 1L", file = file.path(f, "scripts", "a.R"))
  msg <- sprintf("Loading file: %s",
                 file.path(f, "scripts", "a.R"))
  expect_message(load_scripts(f, quiet = FALSE), msg)

})
