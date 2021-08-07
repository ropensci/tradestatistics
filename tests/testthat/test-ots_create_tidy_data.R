context("testthat.R")

# ots_create_tidy_data connects to the API and returns valid tables with a valid input ----

test_that("ots_create_tidy_data connects to the API and returns valid tables with a valid input", {
  vcr::use_cassette(name = "chl_arg_1964_yrpc", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")

    # Bilateral trade Chile-Argentina at commodity level (1964)
    test_data <- ots_create_tidy_data(
      years = 1964, reporters = "chl", partners = "arg", table = "yrpc"
    )
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 15)

    # Bilateral trade Chile-Argentina at aggregated level (1964)
    test_data <- ots_create_tidy_data(
      years = 1964, reporters = "chl", partners = "arg", table = "yrp"
    )
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 7)

    # Chilean trade at commodity level (1964)
    test_data <- ots_create_tidy_data(
      years = 1964, reporters = "chl", table = "yrc"
    )
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 13)

    # Chilean trade at aggregated level (1964)
    test_data <- ots_create_tidy_data(years = 1964, reporters = "chl", table = "yr")
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 21)

    # Commodity trade at aggregated level (1964)
    test_data <- ots_create_tidy_data(years = 1964, table = "yc")
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 11)
  })
})

# ots_create_tidy_data connects to the API and returns valid tables with a valid input and cache ----

test_that("ots_create_tidy_data connects to the API and returns valid tables with a valid input and cache", {
  vcr::use_cassette(name = "chl_arg_1964_yrpc_cache", {
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
    expect_equal(ncol(test_data), 15)
  })
})

# ots_create_tidy_data connects to the API and returns valid tables with a valid input and a commodity filter ----

test_that("ots_create_tidy_data connects to the API and returns valid tables with a valid input and a product filter", {
  vcr::use_cassette(name = "chl_arg_1964_yrpc_wheat", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")

    test_data <- ots_create_tidy_data(
        years = 1964, reporters = "chl", partners = "arg", table = "yrpc",
        commodities = "1101"
      )

    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 15)
  })
})

# ots_create_tidy_data connects to the API and returns valid tables with a valid input and a group filter ----

test_that("ots_create_tidy_data connects to the API and returns valid tables with a valid input and a group filter", {
  vcr::use_cassette(name = "chl_arg_1964_yrpc_fish", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")

    # filter group 03 = fish and crustaceans...
    test_data <- ots_create_tidy_data(
        years = 1964, reporters = "chl", partners = "arg", table = "yrpc",
        commodities = "03"
      )

    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 15)
  })
})

# ots_create_tidy_data connects to the API and returns valid tables with a valid input and a non-used product filter ----

test_that("ots_create_tidy_data connects to the API and returns valid tables with a valid input and a non-used product filter", {
  vcr::use_cassette(name = "chl_arg_1964_yr_apple", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")
    
    test_data <- expect_warning(
      ots_create_tidy_data(years = 1964, table = "yr", commodities = "apple")
    )
    
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 21)
  })
})

# ots_create_tidy_data connects to the API and returns all reporters/partners when those are NULL ----

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

# ots_create_tidy_data connects to the API and returns an observation when no data is found ----

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

# ots_create_tidy_data works with mixed country ISO/string ----

test_that("ots_create_tidy_data works with mixed country ISO/string", {
  vcr::use_cassette(name = "chl_arg_1964_yr", {
    ots_create_tidy_data(
      years = 1964, reporters = c("Argentina","chl"), table = "yr"
    )
    
    ots_create_tidy_data(
      years = 1964, reporters = "mex", partners = c("Canada","usa"), table = "yrp"
    )
  })
})

# ots_create_tidy_data connects to the API and returns an error after invalid y-r-p input ----

test_that("ots_create_tidy_data connects to the API and returns an error after invalid y-r-p input", {
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

# ots_create_tidy_data fails with invalid cache/file input ----

test_that("ots_create_tidy_data fails with invalid cache/file input", {
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

# ots_create_tidy_data fails with a non-existing product code ----

test_that("ots_create_tidy_data fails with a non-existing product code", {
  expect_error(
    ots_create_tidy_data(
      years = 1964, reporters = "chl", partners = "arg", table = "yrpc",
      commodities = "0000"
    )
  )
})

# ots_create_tidy_data fails with a non-existing product string ----

test_that("ots_create_tidy_data fails with a non-existing product string", {
  vcr::use_cassette(name = "chl_arg_1964_yrpc_kriptonite", {
    # Mock countries test inside ots_create_tidy_data
    cli <- crul::HttpClient$new(url = "https://api.tradestatistics.io")
    res <- cli$get("countries/")
    expect_is(res, "HttpResponse")
    
    expect_error(
      expect_warning(
        ots_create_tidy_data(
          years = 1964, reporters = "chl", partners = "arg", table = "yrpc",
          commodities = "kriptonite"
        )
      )
    )
  })
})

# ots_create_tidy_data fails with no country match ----

test_that("ots_create_tidy_data fails with no country match", {
  # Incorrect parameters
  expect_error(
    ots_create_tidy_data(
      years = 1964, reporters = "Wakanda", table = "yr"
    )
  )
  
  # Incorrect parameters
  expect_error(
    ots_create_tidy_data(
      years = 1964, reporters = "usa", partners = "Wakanda", table = "yrp"
    )
  )
})

# ots_create_tidy_data fails with no country string ----

test_that("ots_create_tidy_data fails with no country match", {
  # Incorrect parameters
  expect_error(
    ots_create_tidy_data(
      years = 1964, reporters = "", table = "yr"
    )
  )
})

# ots_create_tidy_data fails with wrong optional parameters ----

test_that("ots_create_tidy_data fails with wrong optional parameters", {
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
      use_localhost = 0
    )
  )
})

# ots_create_tidy_data fails with multiple country match ----

test_that("ots_create_tidy_data fails with multiple country match", {
  # Incorrect parameters
  expect_error(
    # There are multiple matches for the reporters you requested. Please check ots_countries.
    ots_create_tidy_data(
      years = 1964, reporters = "Germany", table = "yr"
    )
  )
  
  expect_error(
    # There are multiple matches for the partners you requested. Please check ots_countries.
    ots_create_tidy_data(
      years = 1964, reporters = "usa", partners = "Germany", table = "yrp"
    )
  )
})
