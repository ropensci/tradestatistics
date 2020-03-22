test_that("ots_product_community works as expected with special cases", {
  expect_error(tradestatistics::ots_product_community())
  expect_error(tradestatistics::ots_product_community("123"))
})
