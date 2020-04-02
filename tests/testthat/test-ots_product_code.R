context("testthat.R")

test_that("ots_product_code works properly for a partial product string matching", {
  test_product <- ots_product_code(productname = "fruit")

  expect_is(test_product, "data.frame")
  expect_equal(nrow(test_product),26)
  expect_equal(ncol(test_product),3)
})

test_that("ots_product_code returns 0 rows for a non-existing product match", {
  d <- ots_product_code(productname = "adamantium")
  expect_equal(nrow(d),0)
})

test_that("ots_product_code returns an error when no product is specified", {
  expect_error(ots_product_code(productname = ""))
})

test_that("ots_product_code works properly for a partial group string matching", {
  test_group <- ots_product_code(productgroup = "vegetable")
  
  expect_is(test_group, "data.frame")
  expect_equal(ncol(test_group),3)
  expect_equal(nrow(test_group),6)
})

test_that("ots_product_code return 0 rows for a non-existing group match", {
  d <- ots_product_code(productgroup = "headphones and speakers")
  expect_equal(nrow(d),0)
})

test_that("ots_product_code returns an error when no group is specified", {
  expect_error(ots_product_code(productgroup = ""))
})

test_that("ots_product_code works ok for both specified product and group", {
  test_both <- ots_product_code(productname = "potato", productgroup = "vegetable")
  
  expect_is(test_both, "data.frame")
  expect_equal(ncol(test_both),6)
  expect_equal(nrow(test_both),2)
})

test_that("ots_product_code fails with NULL product/group", {
  expect_error(ots_product_code(productname = NULL, productgroup = NULL))
})

test_that("ots_product_code fails when both arguments are empty", {
  expect_error(ots_product_code(productname = "", productgroup = ""))
})

test_that("ots_product_code returns error or no results for strange inputs", {
  # this shall fail
  expect_error(ots_product_code(productname = "1234", productgroup = "1234"))
  
  # this shall return an empty data.frame
  d <- ots_product_code(productname = "kriptonite", productgroup = "adamantium")
  expect_is(d, "data.frame")
  expect_equal(nrow(d),0)
})