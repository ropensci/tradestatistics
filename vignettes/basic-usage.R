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

## ----get_countrycode-----------------------------------------------------
# Single match with no replacement
get_countrycode("Chile")

# Single match with replacement
get_countrycode("America")

# Double match with no replacement
get_countrycode("Germany")

## ----get_productcode-----------------------------------------------------
get_productcode("wine")

## ----get_data_1, eval = TRUE---------------------------------------------
# What does Chile exchange with Argentina? (1980)
get_data(years = 1980, reporter = "chl", partner = "arg")

## ----get_data_1_var, eval = FALSE----------------------------------------
#  get_data(years = 1980, reporter = "chl", partner = "arg", table = "yrpc")

## ----get_data_2, eval = FALSE--------------------------------------------
#  # What does Chile exchange with Argentina? (1980-1985)
#  get_data(years = 1980:1985, reporter = "chl", partner = "arg")

## ----get_data_3, eval = FALSE--------------------------------------------
#  # What does Chile exchange with Argentina? (1980-1985)
#  get_data(years = c(1980,1995,2007), reporter = "chl", partner = "arg")

## ----get_data_4, eval = TRUE---------------------------------------------
# What does Chile exchange with Argentina? (1980)
get_data(years = 1980, reporter = "chl", partner = "arg", table = "yrp")

## ----get_data_5, eval = TRUE---------------------------------------------
# What does Chile exchange? (1980)
get_data(years = 1980, reporter = "chl", table = "yrc")

## ----get_data_6, eval = TRUE---------------------------------------------
# What does Chile exchange? (1980)
get_data(years = 1980, reporter = "chl", table = "yr")

## ----get_data_7, eval = TRUE---------------------------------------------
# Which products are exchanged? (1980)
get_data(years = 1980, table = "yc")

