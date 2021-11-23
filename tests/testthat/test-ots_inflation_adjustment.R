context("inflation adjustment")

test_that("ots_inflation_adjustment adjusts the data for yrpc", {
  vcr::use_cassette(name = "chl_arg_1964_yrpc", {
    # Bilateral trade Chile-Argentina at commodity level (1964)
    test_data <- ots_create_tidy_data(
      years = 2004, reporters = "chl", partners = "arg", table = "yrpc"
    )

    test_data_adjusted_backwards <- ots_inflation_adjustment(test_data, reference_year = 2000)

    test_data_adjusted_forwards <- ots_inflation_adjustment(test_data, reference_year = 2004)

    test_data_adjusted_same <- ots_inflation_adjustment(test_data, reference_year = 2008)
    
    expect_is(test_data_adjusted_backwards, "data.frame")
    expect_equal(ncol(test_data_adjusted_backwards), 13)

    expect_is(test_data_adjusted_forwards, "data.frame")
    expect_equal(ncol(test_data_adjusted_forwards), 13)
    
    expect_is(test_data_adjusted_same, "data.frame")
    expect_equal(ncol(test_data_adjusted_same), 13)
  })
})

test_that("ots_inflation_adjustment adjusts the data for yr", {
  vcr::use_cassette(name = "chl_arg_1964_yr", {
    # Bilateral trade Chile-Argentina at commodity level (1964)
    test_data <- ots_create_tidy_data(
      years = 2004, reporters = "chl", partners = "arg", table = "yr"
    )
    
    test_data_adjusted_backwards <- ots_inflation_adjustment(test_data, reference_year = 2000)
    
    expect_is(test_data_adjusted_backwards, "data.frame")
    expect_equal(ncol(test_data_adjusted_backwards), 7)
  })
})

test_that("ots_inflation_adjustment fails if the parameters are null or out of range", {
  vcr::use_cassette(name = "chl_arg_2002_yrp", {
    # Bilateral trade Chile-Argentina at commodity level (1964)
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrp"
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
