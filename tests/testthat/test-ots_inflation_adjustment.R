context("testthat.R")

test_that("ots_inflation_adjustment adjusts the data", {
  vcr::use_cassette(name = "chl_arg_1962", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")

    # Bilateral trade Chile-Argentina at commodity level (1962)
    test_data <- ots_create_tidy_data(
      years = 1962, reporters = "chl", partners = "arg", table = "yrpc"
    )
    
    test_data_adjusted <- test_data %>% 
      ots_inflation_adjustment(reference_year = 2000)
      
    expect_is(test_data_adjusted, "data.frame")
    expect_output(str(test_data_adjusted), "13 variables")
  })
})

test_that("ots_inflation_adjustment fails if the parameters are null or out of range", {
  vcr::use_cassette(name = "chl_arg_1962", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")
    
    # Bilateral trade Chile-Argentina at commodity level (1962)
    test_data <- ots_create_tidy_data(
      years = 1962, reporters = "chl", partners = "arg", table = "yrpc"
    )
    
    expect_error(
      test_data_adjusted <- test_data %>% 
        ots_inflation_adjustment(reference_year = 1776),
      "reference year must be numeric and contained within ots_inflation years range"
    )
    
    expect_error(
      ots_inflation_adjustment(trade_data = NULL, reference_year = 1776),
      "input data cannot be null"
    )
    
    expect_error(
      ots_inflation_adjustment(trade_data = test_data, reference_year = NULL),
      "reference year cannot be null"
    )
  })
})
