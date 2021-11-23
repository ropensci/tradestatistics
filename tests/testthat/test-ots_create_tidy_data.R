context("create tidy data")

# ots_create_tidy_data connects to the API and returns valid tables with a valid input ----

# Mock countries test inside ots_create_tidy_data

test_that("valid input + no cache = yr(p)(c) table", {
  vcr::use_cassette(name = "chl_arg_2002_yrpc", {
    # Bilateral trade Chile-Argentina at commodity level (2002)
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc"
    )
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 11)
    
    # Bilateral trade Chile-Argentina at aggregated level (2002)
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrp"
    )
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 7)
    
    # Chilean trade at commodity level (2002)
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", table = "yrc"
    )
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 9)
    
    # Chilean trade at aggregated level (2002)
    test_data <- ots_create_tidy_data(years = 2002, reporters = "chl", table = "yr")
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 5)
    
    # Commodity trade at aggregated level (2002)
    test_data <- ots_create_tidy_data(years = 2002, table = "yc")
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 7)
  })
})

test_that("valid input + cache = yrpc table", {
  vcr::use_cassette(name = "chl_arg_2002_yrpc_cache", {
    # test in memory cache
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
      use_cache = TRUE
    )
    # test file cache
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
      use_cache = TRUE, file = tempfile("data")
    )
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 11)
  })
})

test_that("valid input + no cache + commodity filter = yrpc table", {
  vcr::use_cassette(name = "chl_arg_2002_yrpc_wheat", {
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
      commodities = "110100"
    )
    
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 11)
  })
})

test_that("valid input + no cache + group filter = yrpc table", {
  vcr::use_cassette(name = "chl_arg_2002_yrpc_fish", {
    # filter group 03 = fish and crustaceans...
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
      commodities = "03"
    )
    
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 11)
  })
})

test_that("unused commodities argument = yr table + warning", {
  vcr::use_cassette(name = "chl_arg_2002_yr_apple", {
    test_data <- expect_warning(
      ots_create_tidy_data(years = 2002, table = "yr", commodities = "apple")
    )
    
    expect_is(test_data, "data.frame")
    expect_equal(ncol(test_data), 5)
  })
})

test_that("valid countries/NULL = yrp table /+ warning", {
  vcr::use_cassette(name = "chl_all_2002_yrp", {
    expect_warning(
      ots_create_tidy_data(
        years = 2002, reporters = "chl", partners = NULL, table = "yrp"
      )
    )
    
    expect_warning(
      ots_create_tidy_data(
        years = 2002, reporters = NULL, partners = 'chl', table = "yrp"
      )
    )
  })
})

test_that("no API data = warning", {
  vcr::use_cassette(name = "chl_myt_2002_yrp", {
    expect_warning(
      ots_create_tidy_data(
        years = 2002, reporters = 'chl', partners = 'myt', table = "yrp"
      )
    )
  })
})

test_that("valid mixed country ISO/string = yrp table", {
  vcr::use_cassette(name = "chl_arg_2002_yr", {
    expect_s3_class(
      ots_create_tidy_data(
        years = 2002, reporters = c("Argentina","chl"), table = "yr"
      ),
      "data.frame"
    )
    
    expect_s3_class(
      ots_create_tidy_data(
        years = 2002, reporters = "mex", partners = c("Canada","usa"), table = "yrp"
      ),
      "data.frame"
    )
  })
})

test_that("valid input = yr groups table", {
  vcr::use_cassette(name = "chl_arg_2002_yr_groups", {
    test_data <- ots_create_tidy_data(
      years = 2002, reporters = "chl", table = "yr-groups"
    )
    
    expect_equal(ncol(test_data), 7)
  })
})

test_that("wrong YR input = error + warning", {
  # Bilateral trade ABC-ARG fake ISO codes (2002) - Error message
  expect_error(
    expect_warning(
      ots_create_tidy_data(years = 2002, reporters = "abc", partners = "arg"),
      "After ignoring the unmatched reporter strings"
    )
  )
  
  # Bilateral trade CHL-ABC fake ISO code (2002) - Error message
  expect_error(
    expect_warning(
      ots_create_tidy_data(years = 2002, reporters = "chl", partners = "abc"),
      "After ignoring the unmatched partner strings"
    )
  )
  
  # Bilateral trade USA (1776) - Error message
  expect_error(
    ots_create_tidy_data(years = 1776, reporters = "usa", partners = "all"),
    "Provided that the table you requested contains a 'year' field"
  )
  
  # Bilateral trade Chile-Argentina with fake table (2002) - Error message
  expect_error(
    ots_create_tidy_data(years = 2002, reporters = "chl", partners = "arg", table = "abc"),
    "requested table does not exist"
  )
})

test_that("invalid cache/file input = error + warning", {
  # Incorrect parameters
  expect_error(
    expect_warning(
      ots_create_tidy_data(
        years = 2002, reporters = "arg", partners = "chl",
        use_cache = 200100,
        file = "foo.bar"
      ),
      "After ignoring the unmatched reporter strings"
    )
  )
  
  expect_error(
    expect_warning(
      ots_create_tidy_data(
        years = 2002, reporters = "arg", partners = "chl",
        use_cache = TRUE,
        file = 200100
      ),
      "After ignoring the unmatched reporter strings"
    )
  )
})

test_that("non-existing product code = error", {
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
      commodities = "0000"
    )
  )
})

test_that("non-existing product string = error + warning", {
  vcr::use_cassette(name = "chl_arg_2002_yrpc", {
    expect_error(
      expect_warning(
        ots_create_tidy_data(
          years = 2002, reporters = "chl", partners = "arg", table = "yrpc",
          commodities = "kriptonite"
        )
      )
    )
  })
})

test_that("no country match = error", {
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "Wakanda", table = "yr"
    )
  )
  
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "usa", partners = "Wakanda", table = "yrp"
    )
  )
  
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "", table = "yr"
    )
  )
})

test_that("wrong optional parameters = error", {
  # Incorrect parameters
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "arg", partners = "chl",
      max_attempts = 0
    )
  )
  
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "arg", partners = "chl",
      use_localhost = 0
    )
  )
})

test_that("multiple country match = error", {
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "Germany", table = "yr"
    )
  )
  
  expect_error(
    ots_create_tidy_data(
      years = 2002, reporters = "usa", partners = "Germany", table = "yrp"
    )
  )
})
