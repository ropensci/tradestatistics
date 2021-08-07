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

## ----commodities2, eval = T---------------------------------------------------
datatable(ots_commodities_shortnames)

## ----commodities3, eval = T---------------------------------------------------
datatable(ots_communities)

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

## ----commodity_code-----------------------------------------------------------
datatable(ots_commodity_community(" ANIMAL "))

## ----yrpc1, eval = T----------------------------------------------------------
yrpc <- ots_create_tidy_data(
  years = 1990,
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
  commodities = c("01", "food"),
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
  commodities = "0101",
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
  commodities = "0808",
  table = "yc"
)

datatable(yc2)

## -----------------------------------------------------------------------------
inflation <- ots_inflation_adjustment(yr, reference_year = 1970)
datatable(inflation)

