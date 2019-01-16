## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----countries, eval = TRUE----------------------------------------------
library(tradestatistics)

countries

## ----products, eval = TRUE-----------------------------------------------
products

## ----country_code--------------------------------------------------------
# Single match with no replacement
country_code("Chile")

# Single match with replacement
country_code("America")

# Double match with no replacement
country_code("Germany")

## ----product_code--------------------------------------------------------
product_code("wine")

## ----trade_data_1, eval = TRUE-------------------------------------------
# What does Chile exchange with Argentina? (1980)
trade_data(years = 1980, reporter = "chl", partner = "arg")

## ----trade_data_1_var, eval = FALSE--------------------------------------
#  trade_data(years = 1980, reporter = "chl", partner = "arg", table = "yrpc")

## ----trade_data_2, eval = FALSE------------------------------------------
#  # What does Chile exchange with Argentina? (1980-1985)
#  trade_data(years = 1980:1985, reporter = "chl", partner = "arg")

## ----trade_data_3, eval = FALSE------------------------------------------
#  # What does Chile exchange with Argentina? (1980-1985)
#  trade_data(years = c(1980,1995,2007), reporter = "chl", partner = "arg")

## ----trade_data_4, eval = TRUE-------------------------------------------
# What does Chile exchange with Argentina? (1980)
trade_data(years = 1980, reporter = "chl", partner = "arg", table = "yrp")

## ----trade_data_5, eval = TRUE-------------------------------------------
# What does Chile exchange? (1980)
trade_data(years = 1980, reporter = "chl", table = "yrc")

## ----trade_data_6, eval = TRUE-------------------------------------------
# What does Chile exchange? (1980)
trade_data(years = 1980, reporter = "chl", table = "yr")

## ----trade_data_7, eval = TRUE-------------------------------------------
# Which products are exchanged? (1980)
trade_data(years = 1980, table = "yc")

