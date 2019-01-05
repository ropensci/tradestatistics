context("test-tradestatistics.R")

test_that("get_product code works properly for a partial matching", {
  test_product <- get_productcode("fruit")
  
  expect_is(test_product, "tbl")
  expect_output(str(test_product), "5 variables")
})