context("testthat.R")

test_that("ots_country_code works properly for a partial product string matching", {
  test_product <- ots_product_code(productname = "fruit")

  expect_is(test_product, "tbl")
  expect_output(str(test_product), "26 obs")
  expect_output(str(test_product), "5 variables")
})

test_that("ots_country_code works properly for a non-existing product match", {
  test_product <- ots_product_code(productname = "adamantium")
  
  expect_is(test_product, "tbl")
  expect_output(str(test_product), "0 obs")
  expect_output(str(test_product), "5 variables")
})

test_that("ots_country_code returns an error when no product is specified", {
  expect_error(
    ots_product_code(productname = ""),
    "nchar\\(productname\\) > 0 is not TRUE"
  )
})

test_that("ots_country_code works properly for a partial group string matching", {
  test_group <- ots_product_code(productgroup = "vegetable")
  
  expect_is(test_group, "tbl")
  expect_output(str(test_group), "58 obs")
  expect_output(str(test_group), "5 variables")
})

test_that("ots_country_code works properly for a non-existing group match", {
  test_group <- ots_product_code(productgroup = "headphones and speakers")
  
  expect_is(test_group, "tbl")
  expect_output(str(test_group), "0 obs")
  expect_output(str(test_group), "5 variables")
})

test_that("ots_country_code returns an error when no group is specified", {
  expect_error(
    ots_product_code(productgroup = ""),
    "nchar\\(productgroup\\) > 0 is not TRUE"
  )
})

test_that("ots_country_code works ok for both specified product and group", {
  test_both <- ots_product_code(productname = "potato", productgroup = "vegetable")
  
  expect_is(test_both, "tbl")
  expect_output(str(test_both), "2 obs")
  expect_output(str(test_both), "6 variables")
})

test_that("ots_country_code returns an error when both arguments are empty", {
  expect_error(
    ots_product_code(productgroup = "", productgroup = ""),
    "formal argument \"productgroup\" matched by multiple actual arguments"
  )
})
