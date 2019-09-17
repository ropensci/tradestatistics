## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  cache = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----tables, eval = T----------------------------------------------------
library(tradestatistics)

ots_tables

## ----countries, eval = T-------------------------------------------------
ots_countries

## ----products, eval = T--------------------------------------------------
ots_products
ots_product_shortnames
ots_communities

## ----inflation, eval = T-------------------------------------------------
ots_inflation

## ----country_code--------------------------------------------------------
# Single match with no replacement
ots_country_code("Chile")

# Single match with replacement
ots_country_code("America")

# Double match with no replacement
ots_country_code("Germany")

## ----product_code--------------------------------------------------------
ots_product_code("wine")

