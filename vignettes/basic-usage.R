## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----tables, eval = T----------------------------------------------------
library(tradestatistics)

ots_attributes_tables

## ----countries, eval = T-------------------------------------------------
ots_attributes_countries

## ----products, eval = T--------------------------------------------------
ots_attributes_countries

## ----country_code--------------------------------------------------------
# Single match with no replacement
ots_country_code("Chile")

# Single match with replacement
ots_country_code("America")

# Double match with no replacement
ots_country_code("Germany")

## ----product_code--------------------------------------------------------
ots_product_code("wine")

## ----yrpc1, eval = T-----------------------------------------------------
ots_create_tidy_data(year = 1980, reporter = "chl", partner = "arg")

## ----yrpc2, eval = T-----------------------------------------------------
ots_create_tidy_data(year = 1980, reporter = "chl", partner = "arg", table = "yrpc")

## ----yrpc3, eval = T-----------------------------------------------------
ots_create_tidy_data(year = 1980:1982, reporter = "chl", partner = "arg")

## ----yrpc4, eval = T-----------------------------------------------------
ots_create_tidy_data(year = c(1980,1981,1984), reporter = "chl", partner = "arg")

## ----yrpc5, eval = T-----------------------------------------------------
ots_create_tidy_data(year = 1980:1981, reporter = "chl", partner = "arg", product = "0101")

## ----yrpc6, eval = T-----------------------------------------------------
ots_create_tidy_data(year = 1980, reporter = "chl", partner = "arg", product = "horse")

## ----yrp1, eval = T------------------------------------------------------
ots_create_tidy_data(year = 1980, reporter = "chl", partner = "arg", table = "yrp")

## ----yrp2, eval = T------------------------------------------------------
ots_create_tidy_data(year = c(1980,1985), reporter = "chl", partner = "arg", table = "yrp")

## ----yrc1, eval = T------------------------------------------------------
ots_create_tidy_data(year = 1980, reporter = "chl", table = "yrc")

## ----yrc2, eval = T------------------------------------------------------
ots_create_tidy_data(year = 1980, reporter = "chl", product = "0101", table = "yrc")

## ----yrc3, eval = T------------------------------------------------------
ots_create_tidy_data(year = 1980, reporter = "chl", product = "horse", table = "yrc")

## ----yr, eval = T--------------------------------------------------------
ots_create_tidy_data(year = 1980, reporter = "chl", table = "yr")

## ----yc1, eval = T-------------------------------------------------------
ots_create_tidy_data(year = 1980, table = "yc")

## ----yc2, eval = T-------------------------------------------------------
ots_create_tidy_data(year = 1980, product = "0101", table = "yc")

## ----yc3, eval = T-------------------------------------------------------
ots_create_tidy_data(year = 1980, product = "horse", table = "yc")

