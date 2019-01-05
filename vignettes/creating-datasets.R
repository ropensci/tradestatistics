## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----tables, message = FALSE, eval = FALSE-------------------------------
#  library(dplyr)
#  
#  tables_tidy_file <- "../data/tables.RData"
#  
#  if (!file.exists(tables_tidy_file)) {
#    tables <- tibble(
#      table = c(
#          "countries",
#          "products",
#          "reporters",
#          "country_rankings",
#          "product_rankings",
#          "yrpc",
#          "yrp",
#          "yrc",
#          "yr",
#          "yc"
#        ),
#      description = c(
#          "Countries metadata",
#          "Product metadata",
#          "Reporting countries",
#          "Ranking of countries",
#          "Ranking of products",
#          "Bilateral trade at commodity level (Year - Reporter - Partner - Commodity)",
#          "Bilateral trade at aggregated level (Year - Reporter - Partner)",
#          "Reporter trade at commodity level (Year - Reporter - Commodity)",
#          "Reporter trade at aggregated level (Year - Reporter)",
#          "Commodity trade at aggregated level (Year - Commodity)"
#        ),
#      example_query = c(
#        "/countries",
#        "/products",
#        "/reporters?y=2016",
#        "/country_rankings?y=2016",
#        "/product_rankings?y=2016",
#        "/yrpc?y=2016&r=chl&p=chn&l=4",
#        "/yrp?y=2016&r=chl&p=chn",
#        "/yrc?y=2016&r=chl&l=4",
#        "/yr?y=2016&r=chl",
#        "/yc?y=2016&l=4"
#      )
#    )
#  
#    save(tables, file = tables_tidy_file, compress = "xz")
#  }

## ----countries, message = FALSE, eval = FALSE----------------------------
#  library(jsonlite)
#  
#  countries_url <- "http://tradestatistics.io:8080/countries"
#  countries_raw_file <- "../data-raw/countries.json"
#  countries_tidy_file <- "../data/countries.RData"
#  
#  if (!file.exists(countries_raw_file)) { download.file(countries_url, countries_raw_file) }
#  
#  if (!file.exists(countries_tidy_file)) {
#    countries <- fromJSON(countries_raw_file) %>% as_tibble()
#  
#    world <- tibble(
#      country_iso = "all",
#      country_name_english = "All Countries",
#      country_fullname_english = "All Countries (this is an alias to show several reporters/partners on API calls)",
#      continent_id = NA,
#      continent = NA,
#      eu28_member = NA
#    )
#  
#    countries <- bind_rows(countries, world)
#  
#    save(countries, file = countries_tidy_file, compress = "xz")
#  }

## ----products, message = FALSE, eval = FALSE-----------------------------
#  products_url <- "http://tradestatistics.io:8080/products"
#  products_raw_file <- "../data-raw/products.json"
#  products_tidy_file <- "../data/products.RData"
#  
#  if (!file.exists(products_raw_file)) { download.file(products_url, products_raw_file) }
#  
#  if (!file.exists(products_tidy_file)) {
#    products <- fromJSON(products_raw_file) %>% as_tibble()
#    save(products, file = products_tidy_file, compress = "xz")
#  }

