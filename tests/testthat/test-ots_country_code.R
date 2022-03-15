context("country code")

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

  expect_is(test_country_1, "data.frame")
  expect_is(test_country_2, "data.frame")
  expect_is(test_country_3, "data.frame")

  expect_equal(ncol(test_country_1), 6)
  expect_equal(ncol(test_country_2), 6)
  expect_equal(ncol(test_country_3), 6)
})

test_that("ots_country_code returns an error when no countryname is specified", {
  expect_error(ots_country_code(countryname = ""))
  expect_error(ots_country_code(countryname = NULL))
})

test_that("ots_country_code returns 0 rows when no match exists", {
  d <- ots_country_code(countryname = "Abc")
  expect_equal(nrow(d), 0)
})
