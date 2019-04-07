## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----tables, message = FALSE, eval = FALSE-------------------------------
#  library(dplyr)
#  library(jsonlite)
#  
#  tables_url <- "https://api.tradestatistics.io/tables"
#  tables_raw_file <- "../data-raw/ots_attributes_tables.json"
#  tables_tidy_file <- "../data/ots_attributes_tables.rda"
#  
#  if (!file.exists(tables_raw_file)) { download.file(tables_url, tables_raw_file) }
#  
#  if (!file.exists(tables_tidy_file)) {
#    ots_attributes_tables <- fromJSON(tables_raw_file) %>%
#      as_tibble() %>%
#      mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
#    save(ots_attributes_tables, file = tables_tidy_file, compress = "xz")
#  }

## ----countries, message = FALSE, eval = FALSE----------------------------
#  countries_url <- "https://api.tradestatistics.io/countries"
#  countries_raw_file <- "../data-raw/ots_attributes_countries.json"
#  countries_tidy_file <- "../data/ots_attributes_countries.rda"
#  
#  if (!file.exists(countries_raw_file)) { download.file(countries_url, countries_raw_file) }
#  
#  if (!file.exists(countries_tidy_file)) {
#    ots_attributes_countries <- fromJSON(countries_raw_file) %>%
#      as_tibble() %>%
#      mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
#    save(ots_attributes_countries, file = countries_tidy_file, compress = "xz")
#  }

## ----products, message = FALSE, eval = FALSE-----------------------------
#  products_url <- "https://api.tradestatistics.io/products"
#  products_raw_file <- "../data-raw/ots_attributes_products.json"
#  products_tidy_file <- "../data/ots_attributes_products.rda"
#  
#  if (!file.exists(products_raw_file)) { download.file(products_url, products_raw_file) }
#  
#  if (!file.exists(products_tidy_file)) {
#    ots_attributes_products <- fromJSON(products_raw_file) %>%
#      as_tibble() %>%
#      mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
#    save(ots_attributes_products, file = products_tidy_file, compress = "xz")
#  }

## ----communities, message = FALSE, eval = FALSE--------------------------
#  communities_url <- "https://api.tradestatistics.io/communities"
#  communities_raw_file <- "../data-raw/ots_attributes_communities.json"
#  communities_tidy_file <- "../data/ots_attributes_communities.rda"
#  
#  if (!file.exists(communities_raw_file)) { download.file(communities_url, communities_raw_file) }
#  
#  if (!file.exists(communities_tidy_file)) {
#    ots_attributes_communities <- fromJSON(communities_raw_file) %>%
#      as_tibble() %>%
#      mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
#    save(ots_attributes_communities, file = communities_tidy_file, compress = "xz")
#  }

## ----product_shortnames, message = FALSE, eval = FALSE-------------------
#  product_shortnames_url <- "https://api.tradestatistics.io/product_shortnames"
#  product_shortnames_raw_file <- "../data-raw/ots_attributes_product_shortnames.json"
#  product_shortnames_tidy_file <- "../data/ots_attributes_product_shortnames.rda"
#  
#  if (!file.exists(product_shortnames_raw_file)) { download.file(product_shortnames_url, product_shortnames_raw_file) }
#  
#  if (!file.exists(product_shortnames_tidy_file)) {
#    ots_attributes_product_shortnames <- fromJSON(product_shortnames_raw_file) %>%
#      as_tibble() %>%
#      mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
#    save(ots_attributes_product_shortnames, file = product_shortnames_tidy_file, compress = "xz")
#  }

