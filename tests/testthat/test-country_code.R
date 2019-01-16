context("testthat.R")

test_that("country_code works properly for single matching", {
  test_country_1 <- country_code("Chile")
  test_country_2 <- country_code("CHILE")
  test_country_3 <- country_code("Chil")
  
  expect_output(str(test_country_1), "chl")
  expect_output(str(test_country_2), "chl")
  expect_output(str(test_country_3), "chl")
})

test_that("country_code works properly for multiple matching", {
  test_country_1 <- country_code("Germany")
  test_country_2 <- country_code("GERMANY")
  test_country_3 <- country_code("all")
  
  expect_is(test_country_1, "tbl")
  expect_is(test_country_2, "tbl")
  expect_is(test_country_3, "character")
  
  expect_output(str(test_country_1), "6 variables")
  expect_output(str(test_country_2), "6 variables")
  expect_output(str(test_country_3), "all")
})
