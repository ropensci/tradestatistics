context("testthat.R")

test_that("ots_trade_data connects to the API and returns valid tables with a 
           valid input", {
  vcr::use_cassette(name = "chl_arg_1980", {
    # Mock countries test inside ots_trade_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")

    # Bilateral trade Chile-Argentina at commodity level (1980)
    test_data <- ots_trade_data(
      years = 1980, reporter = "chl", partner = "arg", table = "yrpc"
    )
    expect_is(test_data, "tbl")
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "20 variables")

    # Bilateral trade Chile-Argentina at aggregated level (1980)
    test_data <- ots_trade_data(
      years = 1980, reporter = "chl", partner = "arg", table = "yrp"
    )
    expect_is(test_data, "tbl")
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "15 variables")

    # Chilean trade at commodity level (1980)
    test_data <- ots_trade_data(
      years = 1980, reporter = "chl", table = "yrc"
    )
    expect_is(test_data, "tbl")
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "20 variables")

    # Chilean trade at aggregated level (1980)
    test_data <- ots_trade_data(years = 1980, reporter = "chl", table = "yr")
    expect_is(test_data, "tbl")
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "21 variables")

    # Commodity trade at aggregated level (1980)
    test_data <- ots_trade_data(years = 1980, table = "yc")
    expect_is(test_data, "tbl")
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "24 variables")
  })
})

test_that("ots_trade_data connects to the API and returns an error after invalid 
           input", {
  # Bilateral trade ABC-CDE fake ISO codes (1980) - Error message
  expect_error(
    ots_trade_data(years = 1980, reporter = "abc", partner = "cde"),
    "'arg' should be one of"
  )

  # Bilateral trade ABC-CDE fake ISO codes (1980) - Error message
  expect_error(
    ots_trade_data(years = 1776, reporter = "usa", partner = "all"),
    "years 1962-2016."
  )

  # Bilateral trade Chile-Argentina with fake table (1980) - Error message
  expect_error(
    ots_trade_data(years = 1980, reporter = "chl", partner = "arg", table = "abc"),
    "The requested table does not exist"
  )
})
