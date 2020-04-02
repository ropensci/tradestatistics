context("testthat.R")

test_that("ots_inflation_adjustment adjusts the data", {
  vcr::use_cassette(name = "chl_arg_1964", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")

    # Bilateral trade Chile-Argentina at commodity level (1964)
    test_data <- ots_create_tidy_data(
      years = 1964, reporters = "chl", partners = "arg", table = "yrpc"
    )

    test_data_adjusted_backwards <- ots_inflation_adjustment(test_data, reference_year = 1962)

    test_data_adjusted_forwards <- ots_inflation_adjustment(test_data, reference_year = 1966)

    expect_is(test_data_adjusted_backwards, "data.frame")
    expect_equal(ncol(test_data_adjusted_backwards), 18)

    expect_is(test_data_adjusted_forwards, "data.frame")
    expect_equal(ncol(test_data_adjusted_forwards), 18)
  })
})

test_that("ots_inflation_adjustment fails if the parameters are null or out of range", {
  vcr::use_cassette(name = "chl_arg_1964", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")

    # Bilateral trade Chile-Argentina at commodity level (1964)
    test_data <- ots_create_tidy_data(
      years = 1964, reporters = "chl", partners = "arg", table = "yrpc"
    )

    # truncated message as it changes when the API has more years
    expect_error(
      test_data_adjusted <- ots_inflation_adjustment(test_data, reference_year = 1776),
      "The reference year must be numeric and contained within ots_inflation years range"
    )

    expect_error(
      ots_inflation_adjustment(trade_data = NULL, reference_year = 1776),
      "The input data cannot be NULL."
    )

    expect_error(
      ots_inflation_adjustment(trade_data = test_data, reference_year = NULL),
      "The reference year cannot be NULL."
    )
  })
})
