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
ots_create_tidy_data(years = 1980, reporters = "chl", partners = "arg")

## ----yrpc2, eval = T-----------------------------------------------------
ots_create_tidy_data(years = 1980, reporters = "chl", partners = "arg", table = "yrpc")

## ----yrpc3, eval = T-----------------------------------------------------
ots_create_tidy_data(years = 1980:1982, reporters = "chl", partners = "arg")

## ----yrpc4, eval = T-----------------------------------------------------
ots_create_tidy_data(years = c(1980,1981,1984), reporters = "chl", partners = "arg")

## ----yrpc5, eval = T-----------------------------------------------------
ots_create_tidy_data(years = 1980:1981, reporters = "chl", partners = "arg", products = "0101")

## ----yrpc6, eval = T-----------------------------------------------------
ots_create_tidy_data(years = 1980, reporters = "chl", partners = "arg", products = "horse")

## ----yrpc7, eval = T-----------------------------------------------------
ots_create_tidy_data(years = 1980, reporters = c("chl", "per"), partners = c("arg", "bra"), products = c("0101", "apple"))

## ----yrpc8, eval = T-----------------------------------------------------
ots_create_tidy_data(years = 1980, reporters = "chl", partners = c("arg", "bra", "per"), products = c("0101", "apple"))

## ----yrp1, eval = T------------------------------------------------------
ots_create_tidy_data(years = 1980, reporters = "chl", partners = "arg", table = "yrp")

## ----yrp2, eval = T------------------------------------------------------
ots_create_tidy_data(years = c(1980,1985), reporters = "chl", partners = "arg", table = "yrp")

## ----yrp3, eval = T------------------------------------------------------
ots_create_tidy_data(years = 1980:1981, reporters = c("chl", "per"), partners = "arg", table = "yrp")

## ----yrc1, eval = T------------------------------------------------------
ots_create_tidy_data(years = 1980, reporters = "chl", table = "yrc")

## ----yrc2, eval = T------------------------------------------------------
ots_create_tidy_data(years = 1980, reporters = "chl", products = "0101", table = "yrc")

## ----yrc3, eval = T------------------------------------------------------
ots_create_tidy_data(years = 1980, reporters = "chl", products = "horse", table = "yrc")

## ----yrc4, eval = T------------------------------------------------------
ots_create_tidy_data(years = c(1980,1985), reporters = c("chl", "per"), products = "apple", table = "yrc")

## ----yr, eval = T--------------------------------------------------------
ots_create_tidy_data(years = 1980, reporters = "chl", table = "yr")

## ----yr2, eval = T-------------------------------------------------------
ots_create_tidy_data(years = c(1980,1985), reporters = c("chl", "arg", "per"), table = "yr")

## ----yc1, eval = T-------------------------------------------------------
ots_create_tidy_data(years = 1980, table = "yc")

## ----yc2, eval = T-------------------------------------------------------
ots_create_tidy_data(years = 1980, products = "0101", table = "yc")

## ----yc3, eval = T-------------------------------------------------------
ots_create_tidy_data(years = 1980, products = "horse", table = "yc")

