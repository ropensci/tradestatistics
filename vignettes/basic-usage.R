## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  cache = FALSE,
  collapse = TRUE,
  message = FALSE,
  comment = "#>"
)

## ----tables, eval = T---------------------------------------------------------
library(tradestatistics)

ots_tables

## ----countries, eval = T------------------------------------------------------
ots_countries

## ----products, eval = T-------------------------------------------------------
ots_products

ots_products_shortnames

## ----products2, eval = T------------------------------------------------------
ots_sections

ots_sections_shortnames

## ----inflation, eval = T------------------------------------------------------
ots_inflation

## ----country_code-------------------------------------------------------------
# Single match with no replacement
ots_country_code("Chile")

# Single match with replacement
ots_country_code("America")

# Double match with no replacement
ots_country_code("Germany")

## ----product_code-------------------------------------------------------------
ots_product_code(" WiNe ")

## ----product_code2------------------------------------------------------------
ots_product_code(productname = " ShEEp ", productgroup = " mEaT ")

## ----section_code-------------------------------------------------------------
ots_product_section(" tExTiLeS ")

## ----yrpc1, eval = T----------------------------------------------------------
ots_create_tidy_data(
  years = 1962,
  reporters = "chl",
  partners = "arg",
  table = "yrpc"
)

## ----yrpc2, eval = T----------------------------------------------------------
# Note that here I'm passing Peru and not per which is the ISO code for Peru
# The same applies to Brazil
ots_create_tidy_data(
  years = c(1962,1963),
  reporters = c("chl", "Peru", "bol"),
  partners = c("arg", "Brazil"),
  sections = c("01", "food"),
  table = "yrpc"
)

## ----yrp3, eval = T-----------------------------------------------------------
ots_create_tidy_data(
  years = 1962:1963,
  reporters = c("chl", "per"),
  partners = "arg",
  table = "yrp"
)

## ----yrc2, eval = T-----------------------------------------------------------
ots_create_tidy_data(
  years = 1962,
  reporters = "chl",
  products = "0101",
  table = "yrc"
)

## ----yr2, eval = T------------------------------------------------------------
yr <- ots_create_tidy_data(
  years = 1962:1963,
  reporters = c("chl", "arg", "per"),
  table = "yr"
)

yr

## ----yc1, eval = T------------------------------------------------------------
ots_create_tidy_data(
  years = 1962,
  table = "yc"
)

## ----yc2, eval = T------------------------------------------------------------
ots_create_tidy_data(
  years = 1962,
  products = "0101",
  table = "yc"
)

## -----------------------------------------------------------------------------
ots_inflation_adjustment(yr, reference_year = 1970)

