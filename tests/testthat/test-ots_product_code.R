context("testthat.R")

test_that("ots_country_code works properly for a partial string matching", {
  test_product <- ots_product_code(productname = "fruit")

  expect_is(test_product, "tbl")
  expect_output(str(test_product), "102 obs")
  expect_output(str(test_product), "5 variables")
})

test_that("ots_country_code works properly for a non-existing match", {
  test_product <- ots_product_code(productname = "adamantium")
  
  expect_is(test_product, "tbl")
  expect_output(str(test_product), "0 obs")
  expect_output(str(test_product), "5 variables")
})

test_that("ots_country_code returns an error when no productname is specified", {
  expect_error(
    ots_product_code(productname = ""),
    "nchar\\(productname\\) > 0 is not TRUE"
  )
})
