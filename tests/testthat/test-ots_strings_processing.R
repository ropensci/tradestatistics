context("strings processing")

test_that("ots_commodity_code works properly for a partial product string matching", {
  test_product <- ots_commodity_code(commodity = "fruit")

  expect_is(test_product, "data.frame")
  expect_equal(nrow(test_product),26)
  expect_equal(ncol(test_product),4)
})

test_that("ots_commodity_code returns 0 rows for a non-existing product match", {
  d <- ots_commodity_code(commodity = "adamantium")
  expect_equal(nrow(d),0)
})

test_that("ots_commodity_code returns an error when no product is specified", {
  expect_error(ots_commodity_code(commodity = ""))
})

test_that("ots_commodity_code works properly for a partial group string matching", {
  test_group <- ots_commodity_code(group = "vegetable")
  
  expect_is(test_group, "data.frame")
  expect_equal(ncol(test_group),2)
  expect_equal(nrow(test_group),6)
})

test_that("ots_commodity_code return 0 rows for a non-existing group match", {
  d <- ots_commodity_code(group = "headphones and speakers")
  expect_equal(nrow(d),0)
})

test_that("ots_commodity_code returns an error when no group is specified", {
  expect_error(ots_commodity_code(group = ""))
})

test_that("ots_commodity_code works ok for both specified product and group", {
  test_both <- ots_commodity_code(commodity = "potato", group = "vegetable")
  
  expect_is(test_both, "data.frame")
  expect_equal(ncol(test_both),4)
  expect_equal(nrow(test_both),2)
})

test_that("ots_commodity_code fails with NULL product/group", {
  expect_error(ots_commodity_code(commodity = NULL, group = NULL))
})

test_that("ots_commodity_code fails when both arguments are empty", {
  expect_error(ots_commodity_code(commodity = "", group = ""))
})

test_that("ots_commodity_code returns error or no results for strange inputs", {
  # this shall fail
  expect_error(ots_commodity_code(commodity = "1234", group = "1234"))
  
  # this shall return an empty data.frame
  d <- ots_commodity_code(commodity = "kriptonite", group = "adamantium")
  expect_is(d, "data.frame")
  expect_equal(nrow(d),0)
})

test_that("ots_product_community works as expected with common cases", {
  expect_is(tradestatistics::ots_commodity_community("animal"), "data.frame")
  expect_is(tradestatistics::ots_commodity_community("vegetable"), "data.frame")
})

test_that("ots_product_community works as expected with special cases", {
  expect_error(tradestatistics::ots_commodity_community())
  expect_error(tradestatistics::ots_commodity_community("123"))
})
