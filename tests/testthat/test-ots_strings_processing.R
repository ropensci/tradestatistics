context("strings processing")

test_that("ots_commodity_code works properly for a partial product string matching", {
  test_product <- ots_commodity_code(commodity = "fruit")

  expect_is(test_product, "data.frame")
  expect_equal(nrow(test_product),89)
  expect_equal(ncol(test_product),2)
})

test_that("ots_commodity_code returns 0 rows for a non-existing product match", {
  d <- ots_commodity_code(commodity = "adamantium")
  expect_equal(nrow(d),0)
})

test_that("ots_commodity_code returns an error when no product is specified", {
  expect_error(ots_commodity_code(commodity = ""))
})

test_that("ots_commodity_code works properly for a partial section string matching", {
  test_section <- ots_commodity_code(section = "vegetable")
  
  expect_is(test_section, "data.frame")
  expect_equal(ncol(test_section),2)
  expect_equal(nrow(test_section),2)
})

test_that("ots_commodity_code return 0 rows for a non-existing section match", {
  d <- ots_commodity_code(section = "headphones and speakers")
  expect_equal(nrow(d),0)
})

test_that("ots_commodity_code returns an error when no section is specified", {
  expect_error(ots_commodity_code(section = ""))
})

test_that("ots_commodity_code works ok for both specified product and section", {
  test_both <- ots_commodity_code(commodity = "potato", section = "vegetable")
  
  expect_is(test_both, "data.frame")
  expect_equal(ncol(test_both),5)
  expect_equal(nrow(test_both),8)
})

test_that("ots_commodity_code fails with NULL product/section", {
  expect_error(ots_commodity_code(commodity = NULL, section = NULL))
})

test_that("ots_commodity_code fails when both arguments are empty", {
  expect_error(ots_commodity_code(commodity = "", section = ""))
})

test_that("ots_commodity_code returns error or no results for strange inputs", {
  # this shall fail
  expect_error(ots_commodity_code(commodity = "1234", section = "1234"))
  
  # this shall return an empty data.frame
  d <- ots_commodity_code(commodity = "kriptonite", section = "adamantium")
  expect_is(d, "data.frame")
  expect_equal(nrow(d),0)
})
