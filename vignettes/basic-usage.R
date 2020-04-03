## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  cache = FALSE,
  collapse = TRUE,
  message = FALSE,
  comment = "#>"
)

datatable <- function(x) {
  DT::datatable(x,
    extensions = "FixedColumns",
    options = list(
      pageLength = 5,
      dom = 'Bfrtip',
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 2, rightColumns = 1)
    )
)}

## ----pkgs---------------------------------------------------------------------
library(tradestatistics)
library(DT)

## ----tables, eval = T---------------------------------------------------------
datatable(ots_tables)

## ----countries, eval = T------------------------------------------------------
datatable(ots_countries)

## ----products, eval = T-------------------------------------------------------
datatable(ots_products)

## ----products2, eval = T------------------------------------------------------
datatable(ots_products_shortnames)

## ----products3, eval = T------------------------------------------------------
datatable(ots_sections)

## ----products4, eval = T------------------------------------------------------
datatable(ots_sections_shortnames)

## ----inflation, eval = T------------------------------------------------------
datatable(ots_inflation)

## ----country_code-------------------------------------------------------------
# Single match with no replacement
datatable(ots_country_code("Chile"))

# Single match with replacement
datatable(ots_country_code("America"))

# Double match with no replacement
datatable(ots_country_code("Germany"))

## ----product_code-------------------------------------------------------------
datatable(ots_product_code(" WiNe "))

## ----product_code2------------------------------------------------------------
datatable(ots_product_code(productname = " ShEEp ", productgroup = " mEaT "))

## ----section_code-------------------------------------------------------------
datatable(ots_product_section(" tExTiLeS "))

## ----yrpc1, eval = T----------------------------------------------------------
yrpc <- ots_create_tidy_data(
  years = 1962,
  reporters = "chl",
  partners = "arg",
  table = "yrpc"
)

datatable(yrpc)

## ----yrpc2, eval = T----------------------------------------------------------
# Note that here I'm passing Peru and not per which is the ISO code for Peru
# The same applies to Brazil
yrpc2 <- ots_create_tidy_data(
  years = c(1962,1963),
  reporters = c("chl", "Peru", "bol"),
  partners = c("arg", "Brazil"),
  sections = c("01", "food"),
  table = "yrpc"
)
datatable(yrpc2)

## ----yrp3, eval = T-----------------------------------------------------------
yrp <- ots_create_tidy_data(
  years = 1962:1963,
  reporters = c("chl", "per"),
  partners = "arg",
  table = "yrp"
)

datatable(yrp)

## ----yrc2, eval = T-----------------------------------------------------------
yrc <- ots_create_tidy_data(
  years = 1962,
  reporters = "chl",
  products = "0101",
  table = "yrc"
)

datatable(yrc)

## ----yr2, eval = T------------------------------------------------------------
yr <- ots_create_tidy_data(
  years = 1962:1963,
  reporters = c("chl", "arg", "per"),
  table = "yr"
)

datatable(yr)

## ----yc1, eval = T------------------------------------------------------------
yc <- ots_create_tidy_data(
  years = 1962,
  table = "yc"
)

datatable(yc)

## ----yc2, eval = T------------------------------------------------------------
yc2 <- ots_create_tidy_data(
  years = 1962,
  products = "0101",
  table = "yc"
)

datatable(yc2)

## -----------------------------------------------------------------------------
inflation <- ots_inflation_adjustment(yr, reference_year = 1970)
datatable(inflation)

