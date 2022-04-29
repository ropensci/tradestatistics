## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  cache = FALSE,
  collapse = TRUE,
  message = FALSE,
  comment = "#>"
)

## ----pkgs---------------------------------------------------------------------
library(tradestatistics)
library(tibble)

## ----tables, eval = T---------------------------------------------------------
as_tibble(ots_tables)

## ----countries, eval = T------------------------------------------------------
as_tibble(ots_countries)

## ----commodities, eval = T----------------------------------------------------
as_tibble(ots_commodities)

## ----inflation, eval = T------------------------------------------------------
as_tibble(ots_gdp_deflator)

## ----country_code-------------------------------------------------------------
# Single match with no replacement
as_tibble(ots_country_code("Chile"))

# Single match with replacement
as_tibble(ots_country_code("America"))

# Double match with no replacement
as_tibble(ots_country_code("Germany"))

## ----commodity_code2----------------------------------------------------------
as_tibble(ots_commodity_code(commodity = " ShEEp ", section = " mEaT "))

## ----yrpc1, eval = F----------------------------------------------------------
#  yrpc <- ots_create_tidy_data(
#    years = 2019,
#    reporters = "chl",
#    partners = "arg",
#    table = "yrpc"
#  )
#  
#  as_tibble(yrpc)

## ----yrpc2, echo=FALSE--------------------------------------------------------
as_tibble(tradestatistics:::ots_demo_data$yrpc)

## ----yrpc3, eval = F----------------------------------------------------------
#  # Note that here I'm passing Peru and not per which is the ISO code for Peru
#  # The same applies to Brazil
#  yrpc2 <- ots_create_tidy_data(
#    years = 2018:2019,
#    reporters = c("chl", "Peru", "bol"),
#    partners = c("arg", "Brazil"),
#    commodities = c("01", "food"),
#    table = "yrpc"
#  )

## ----yrp3, eval = F-----------------------------------------------------------
#  yrp <- ots_create_tidy_data(
#    years = 2018:2019,
#    reporters = c("chl", "per"),
#    partners = "arg",
#    table = "yrp"
#  )

## ----yrc2, eval = F-----------------------------------------------------------
#  yrc <- ots_create_tidy_data(
#    years = 2019,
#    reporters = "chl",
#    commodities = "010121",
#    table = "yrc"
#  )

## ----yr2, eval = F------------------------------------------------------------
#  yr <- ots_create_tidy_data(
#    years = 2018:2019,
#    reporters = c("chl", "arg", "per"),
#    table = "yr"
#  )

## ----yc1, eval = F------------------------------------------------------------
#  yc <- ots_create_tidy_data(
#    years = 2019,
#    table = "yc"
#  )

## ----yc2, eval = F------------------------------------------------------------
#  yc2 <- ots_create_tidy_data(
#    years = 2019,
#    commodities = "010121",
#    table = "yc"
#  )

## ----inflation2, eval=FALSE---------------------------------------------------
#  inflation <- ots_gdp_deflator_adjustment(yr, reference_year = 2000)
#  as_tibble(inflation)

## ----inflation3, echo=FALSE---------------------------------------------------
as_tibble(ots_gdp_deflator_adjustment(tradestatistics:::ots_demo_data$yr, reference_year = 2000))

