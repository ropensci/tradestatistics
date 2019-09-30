context("testthat.R")

test_that("ots_country_code works properly for single matching", {
  test_country_1 <- ots_country_code("Chile")
  test_country_2 <- ots_country_code("CHILE")
  test_country_3 <- ots_country_code("Chil")

  expect_output(str(test_country_1), "chl")
  expect_output(str(test_country_2), "chl")
  expect_output(str(test_country_3), "chl")
})

test_that("ots_country_code works properly for multiple matching", {
  test_country_1 <- ots_country_code("Germany")
  test_country_2 <- ots_country_code("GERMANY")
  test_country_3 <- ots_country_code("all")

  expect_is(test_country_1, "tbl")
  expect_is(test_country_2, "tbl")
  expect_is(test_country_3, "tbl")

  expect_output(str(test_country_1), "6 variables")
  expect_output(str(test_country_2), "6 variables")
  expect_output(str(test_country_3), "6 variables")
})

test_that("ots_country_code returns an error when no countryname is specified", {
  expect_error(
    ots_country_code(countryname = ""),
    "countryname can't have zero characters after removing numbers"
  )
  
  expect_error(
    ots_country_code(countryname = NULL),
    "countryname can't be NULL"
  )
})

test_that("ots_country_code returns an error when no match exists", {
  expect_error(
    ots_country_code(countryname = "Abc"),
    "no match for your search"
  )
})
