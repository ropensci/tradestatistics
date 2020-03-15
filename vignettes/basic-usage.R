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

ots_product_shortnames

ots_communities

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
ots_product_code("wine")

## ----product_code2------------------------------------------------------------
ots_product_code(productname = "Milk", productgroup = "Dairy")

## ----community_code-----------------------------------------------------------
ots_product_community("fabric")

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
  table = "yrpc"
)

## ----yrpc3, eval = T----------------------------------------------------------
# Pass a specific HS code
ots_create_tidy_data(
  years = c(1962,1963),
  reporters = c("chl", "Peru", "bol"),
  partners = c("arg", "bra"), 
  products = "0101",
  table = "yrpc"
)

# Pass a string that will return all matching descriptions and multiple HS codes
ots_create_tidy_data(
  years = c(1962,1963),
  reporters = c("chl", "Peru", "bol"),
  partners = c("arg", "bra"),
  products = c("0101", "apple"),
  table = "yrpc"
)

## ----yrpg1, eval = T----------------------------------------------------------
ots_create_tidy_data(
  years = 1962,
  reporters = "chl",
  partners = "arg",
  table = "yrpg"
)

## ----yrpg2, eval = T----------------------------------------------------------
# Note that here I'm passing Peru and not per which is the ISO code for Peru
# The same applies to Brazil
ots_create_tidy_data(
  years = c(1962,1963),
  reporters = c("chl", "Peru", "bol"),
  partners = c("arg", "Brazil"),
  table = "yrpg"
)

## ----yrpg3, eval = T----------------------------------------------------------
# Pass a specific group code
ots_create_tidy_data(
  years = c(1962,1963),
  reporters = c("chl", "Peru", "bol"),
  partners = c("arg", "bra"), 
  groups = "01",
  table = "yrpg"
)

# Pass a string that will return all matching descriptions and multiple groups
ots_create_tidy_data(
  years = c(1962,1963),
  reporters = c("chl", "Peru", "bol"),
  partners = c("arg", "bra"),
  groups = c("01", "food"),
  table = "yrpg"
)

## ----yrp1, eval = T-----------------------------------------------------------
ots_create_tidy_data(
  years = 1962,
  reporters = "chl",
  partners = "arg",
  table = "yrp"
)

## ----yrp3, eval = T-----------------------------------------------------------
ots_create_tidy_data(
  years = 1962:1963,
  reporters = c("chl", "per"),
  partners = "arg",
  table = "yrp"
)

## ----yrc1, eval = T-----------------------------------------------------------
ots_create_tidy_data(
  years = 1962,
  reporters = "chl",
  table = "yrc"
)

## ----yrc2, eval = T-----------------------------------------------------------
ots_create_tidy_data(
  years = 1962,
  reporters = "chl",
  products = "0101",
  table = "yrc"
)

## ----yr, eval = T-------------------------------------------------------------
ots_create_tidy_data(
  years = 1962,
  reporters = "chl",
  table = "yr"
)

## ----yr2, eval = T------------------------------------------------------------
ots_create_tidy_data(
  years = 1962:1963,
  reporters = c("chl", "arg", "per"),
  table = "yr"
)

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
d <- ots_create_tidy_data(
  years = 1962,
  reporters = "chl",
  table = "yr"
) 

ots_inflation_adjustment(d, reference_year = 1970)

