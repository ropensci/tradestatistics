context("testthat.R")

test_that("ots_create_tidy_data connects to the API and returns valid tables with a 
           valid input", {
  vcr::use_cassette(name = "chl_arg_1962", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")

    # Bilateral trade Chile-Argentina at commodity level (1962)
    test_data <- ots_create_tidy_data(
      years = 1962, reporters = "chl", partners = "arg", table = "yrpc"
    )
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "10 variables")

    # Bilateral trade Chile-Argentina at aggregated level (1962)
    test_data <- ots_create_tidy_data(
      years = 1962, reporters = "chl", partners = "arg", table = "yrp"
    )
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "7 variables")

    # Chilean trade at commodity level (1962)
    test_data <- ots_create_tidy_data(
      years = 1962, reporters = "chl", table = "yrc"
    )
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "10 variables")

    # Chilean trade at aggregated level (1962)
    test_data <- ots_create_tidy_data(years = 1962, reporters = "chl", table = "yr")
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "15 variables")

    # Commodity trade at aggregated level (1962)
    test_data <- ots_create_tidy_data(years = 1962, table = "yc")
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "17 variables")
  })
})

test_that("ots_create_tidy_data connects to the API and returns an error after invalid 
           input", {
  # Bilateral trade ABC-CDE fake ISO codes (1962) - Error message
  expect_error(
    ots_create_tidy_data(years = 1962, reporters = "abc", partners = "cde"),
    "no valid ISO code"
  )

  # Bilateral trade USA (1776) - Error message
  expect_error(
    ots_create_tidy_data(years = 1776, reporters = "usa", partners = "all"),
    "years exposed in api.tradestatistics.io/year_range."
  )

  # Bilateral trade Chile-Argentina with fake table (1962) - Error message
  expect_error(
    ots_create_tidy_data(years = 1962, reporters = "chl", partners = "arg", table = "abc"),
    "requested table does not exist"
  )
})
