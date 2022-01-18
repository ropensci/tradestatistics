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

## ----commodities, eval = T----------------------------------------------------
datatable(ots_commodities)

## ----inflation, eval = T------------------------------------------------------
datatable(ots_inflation)

## ----country_code-------------------------------------------------------------
# Single match with no replacement
datatable(ots_country_code("Chile"))

# Single match with replacement
datatable(ots_country_code("America"))

# Double match with no replacement
datatable(ots_country_code("Germany"))

## ----commodity_code2----------------------------------------------------------
datatable(ots_commodity_code(commodity = " ShEEp ", group = " mEaT "))

## ----yrpc1, eval = T----------------------------------------------------------
yrpc <- ots_create_tidy_data(
  years = 2019,
  reporters = "chl",
  partners = "arg",
  table = "yrpc",
  use_localhost = FALSE
)

datatable(yrpc)

## ----yrpc2, eval = T----------------------------------------------------------
# Note that here I'm passing Peru and not per which is the ISO code for Peru
# The same applies to Brazil
yrpc2 <- ots_create_tidy_data(
  years = 2018:2019,
  reporters = c("chl", "Peru", "bol"),
  partners = c("arg", "Brazil"),
  commodities = c("01", "food"),
  table = "yrpc",
  use_localhost = FALSE
)
datatable(yrpc2)

## ----yrp3, eval = T-----------------------------------------------------------
yrp <- ots_create_tidy_data(
  years = 2018:2019,
  reporters = c("chl", "per"),
  partners = "arg",
  table = "yrp",
  use_localhost = FALSE
)

datatable(yrp)

## ----yrc2, eval = T-----------------------------------------------------------
yrc <- ots_create_tidy_data(
  years = 2019,
  reporters = "chl",
  commodities = "010121",
  table = "yrc",
  use_localhost = FALSE
)

datatable(yrc)

## ----yr2, eval = T------------------------------------------------------------
yr <- ots_create_tidy_data(
  years = 2018:2019,
  reporters = c("chl", "arg", "per"),
  table = "yr",
  use_localhost = FALSE
)

datatable(yr)

## ----yc1, eval = T------------------------------------------------------------
yc <- ots_create_tidy_data(
  years = 2019,
  table = "yc",
  use_localhost = FALSE
)

datatable(yc)

## ----yc2, eval = T------------------------------------------------------------
yc2 <- ots_create_tidy_data(
  years = 2019,
  commodities = "010121",
  table = "yc",
  use_localhost = FALSE
)

datatable(yc2)

## -----------------------------------------------------------------------------
inflation <- ots_inflation_adjustment(yr, reference_year = 2000)
datatable(inflation)

