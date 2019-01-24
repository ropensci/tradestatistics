context("testthat.R")

test_that("ots_country_code works properly for a partial matching", {
  test_product <- ots_product_code("fruit")

  expect_is(test_product, "tbl")
  expect_output(str(test_product), "5 variables")
})
