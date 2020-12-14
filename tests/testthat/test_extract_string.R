
test_that("extract_string returned expected results", {
  x <- "I love RECON"
  pattern <- "[A-Z]{5}"
  expected <- "RECON"
  actual <- extract_string(x, pattern)
  expect_equal(expected, unname(actual))
})





test_that("extract_string returns the correct class", {
  x <- "I love RECON"
  pattern <- "[A-Z]{5}"
  expect_type(extract_string(x, pattern), "character")
})






test_that("extract_string  handles vectors of characters", {
  x <- c("asd", "45", "4555asd")
  expected <- c(NA, NA, "5a")
  actual <- extract_string(x, pattern = "[0-9][:alpha:]")
  expect_equal(expected, unname(actual))
})
