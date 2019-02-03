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

## ----trade_data_1, eval = T----------------------------------------------
# What does Chile exchange with Argentina? (1980)
ots_create_tidy_data(years = 1980, reporter = "chl", partner = "arg")

## ----trade_data_1_var, eval = T------------------------------------------
ots_create_tidy_data(years = 1980, reporter = "chl", partner = "arg", table = "yrpc")

## ----trade_data_2, eval = T----------------------------------------------
# What does Chile exchange with Argentina? (1980-1985)
ots_create_tidy_data(years = 1980:1985, reporter = "chl", partner = "arg")

## ----trade_data_3, eval = T----------------------------------------------
# What does Chile exchange with Argentina? (1980-1985)
ots_create_tidy_data(years = c(1980, 1995, 2007), reporter = "chl", partner = "arg")

## ----trade_data_4, eval = T----------------------------------------------
# What does Chile exchange with Argentina? (1980)
ots_create_tidy_data(years = 1980, reporter = "chl", partner = "arg", table = "yrp")

## ----trade_data_5, eval = T----------------------------------------------
# What does Chile exchange? (1980)
ots_create_tidy_data(years = 1980, reporter = "chl", table = "yrc")

## ----trade_data_6, eval = T----------------------------------------------
# What does Chile exchange? (1980)
ots_create_tidy_data(years = 1980, reporter = "chl", table = "yr")

## ----trade_data_7, eval = T----------------------------------------------
# Which products are exchanged? (1980)
ots_create_tidy_data(years = 1980, table = "yc")
