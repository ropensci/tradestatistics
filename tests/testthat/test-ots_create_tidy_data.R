context("testthat.R")

test_that("ots_create_tidy_data connects to the API and returns valid tables with a valid input", {
  vcr::use_cassette(name = "chl_arg_1964", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")

    # Bilateral trade Chile-Argentina at commodity level (1964)
    test_data <- ots_create_tidy_data(
      years = 1964, reporters = "chl", partners = "arg", table = "yrpc"
    )
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "11 variables")

    test_data <- ots_create_tidy_data(
      years = 1964, reporters = "chl", partners = "arg", table = "yrpc",
      include_shortnames = T, include_communities = T
    )
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "15 variables")

    # Bilateral trade Chile-Argentina at aggregated level (1964)
    test_data <- ots_create_tidy_data(
      years = 1964, reporters = "chl", partners = "arg", table = "yrp"
    )
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "7 variables")

    # Chilean trade at commodity level (1964)
    test_data <- ots_create_tidy_data(
      years = 1964, reporters = "chl", table = "yrc"
    )
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "11 variables")

    # Chilean trade at aggregated level (1964)
    test_data <- ots_create_tidy_data(years = 1964, reporters = "chl", table = "yr")
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "15 variables")

    # Commodity trade at aggregated level (1964)
    test_data <- ots_create_tidy_data(years = 1964, table = "yc")
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "17 variables")
  })
})

test_that("ots_create_tidy_data connects to the API and returns valid tables with a valid input", {
  vcr::use_cassette(name = "chl_arg_1964", {
    # test in memory cache
    test_data <- ots_create_tidy_data(
      years = 1964, reporters = "chl", partners = "arg", table = "yrpc",
      use_cache = TRUE
    )
    # test file cache
    test_data <- ots_create_tidy_data(
      years = 1964, reporters = "chl", partners = "arg", table = "yrpc",
      use_cache = TRUE, file = tempfile("data")
    )
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "11 variables")
  })
})

test_that("ots_create_tidy_data connects to the API and returns valid tables with a valid input and a product filter", {
  vcr::use_cassette(name = "chl_arg_1964_yrpc_apple", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")
    
    test_data <- ots_create_tidy_data(
        years = 1964, reporters = "chl", partners = "arg", table = "yrpc",
        products = "apple"
      )
    
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "11 variables")
  })
})

test_that("ots_create_tidy_data connects to the API and returns valid tables with a valid input and a non-used product filter", {
  vcr::use_cassette(name = "chl_arg_1964_yr_apple", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")
    
    test_data <- expect_warning(
      ots_create_tidy_data(years = 1964, table = "yr", products = "apple")
    )
    
    expect_is(test_data, "data.frame")
    expect_output(str(test_data), "15 variables")
  })
})

test_that("ots_create_tidy_data connects to the API and returns all reporters/partners when those are NULL", {
  vcr::use_cassette(name = "chl_all_1964", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")
    
    expect_warning(
      ots_create_tidy_data(
        years = 1964, reporters = "chl", partners = NULL, table = "yrp"
      )
    )
    
    expect_warning(
      ots_create_tidy_data(
        years = 1964, reporters = NULL, partners = 'chl', table = "yrp"
      )
    )
  })
})

test_that("ots_create_tidy_data connects to the API and returns an observation when no data is found", {
  vcr::use_cassette(name = "chl_prk_1964", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")
    
    expect_warning(
      ots_create_tidy_data(
        years = 1964, reporters = 'chl', partners = 'prk', table = "yrp"
      )
    )
  })
})

test_that("ots_create_tidy_data fails with a non-existing product", {
  expect_error(
    ots_create_tidy_data(
      years = 1964, reporters = "chl", partners = "arg", table = "yrpc",
      products = "0000"
    )
  )
})

test_that("ots_create_tidy_data fails with a non-existing product", {
  vcr::use_cassette(name = "chl_arg_1964_kriptonite", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")
    
    expect_error(
      expect_warning(
        ots_create_tidy_data(
          years = 1964, reporters = "chl", partners = "arg", table = "yrpc",
          products = "kriptonite"
        )
      )
    )
  })
})
  
test_that("ots_create_tidy_data connects to the API and returns an error after invalid input", {
  # Bilateral trade ABC-ARG fake ISO codes (1964) - Error message
  expect_error(
    expect_warning(
      ots_create_tidy_data(years = 1964, reporters = "abc", partners = "arg"),
      "After ignoring the unmatched reporter strings"
      )
  )

  # Bilateral trade CHL-ABC fake ISO code (1964) - Error message
  expect_error(
    expect_warning(
      ots_create_tidy_data(years = 1964, reporters = "chl", partners = "abc"),
      "After ignoring the unmatched partner strings"
    )
  )

  # Bilateral trade USA (1776) - Error message
  expect_error(
    ots_create_tidy_data(years = 1776, reporters = "usa", partners = "all"),
    "Provided that the table you requested contains a 'year' field"
  )

  # Bilateral trade Chile-Argentina with fake table (1964) - Error message
  expect_error(
    ots_create_tidy_data(years = 1964, reporters = "chl", partners = "arg", table = "abc"),
    "requested table does not exist"
  )
})

test_that("ots_create_tidy_data returns an error after invalid input", {
  # Incorrect parameters
  expect_error(
    expect_warning(
      ots_create_tidy_data(
        years = 1964, reporters = "arg", partners = "chl",
        use_cache = 200100,
        file = "foo.bar"
      ),
      "After ignoring the unmatched reporter strings"
    )
  )
  
  expect_error(
    expect_warning(
      ots_create_tidy_data(
        years = 1964, reporters = "arg", partners = "chl",
        use_cache = TRUE,
        file = 200100
      ),
      "After ignoring the unmatched reporter strings"
    )
  )
})

test_that("ots_create_tidy_data breaks with wrong optional parameters", {
  # Incorrect parameters
  expect_error(
    ots_create_tidy_data(
      years = 1964, reporters = "arg", partners = "chl",
      max_attempts = 0
    )
  )
  
  expect_error(
    ots_create_tidy_data(
      years = 1964, reporters = "arg", partners = "chl",
      include_shortnames = 0
    )
  )
  
  expect_error(
    ots_create_tidy_data(
      years = 1964, reporters = "arg", partners = "chl",
      include_communities = 0
    )
  )
  
  expect_error(
    ots_create_tidy_data(
      years = 1964, reporters = "arg", partners = "chl",
      use_localhost = 0
    )
  )
})
