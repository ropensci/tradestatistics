# Tables

library(dplyr)
library(jsonlite)

# base_url <- "https://api.tradestatistics.io/"
base_url <- "http://127.0.0.1:8080/"
  
tables_url <- paste0(base_url, "tables")
tables_raw_file <- "data-raw/ots_tables.json"
tables_tidy_file <- "data/ots_tables.rda"

if (!file.exists(tables_raw_file)) { download.file(tables_url, tables_raw_file) }

if (!file.exists(tables_tidy_file)) {
  ots_tables <- fromJSON(tables_raw_file) %>% 
    as_tibble() %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  save(ots_tables, file = tables_tidy_file, version = 2)
}


# Country codes

countries_url <- paste0(base_url, "countries")
countries_raw_file <- "data-raw/ots_countries.json"
countries_tidy_file <- "data/ots_countries.rda"

if (!file.exists(countries_raw_file)) { download.file(countries_url, countries_raw_file) }

if (!file.exists(countries_tidy_file)) {
  ots_countries <- fromJSON(countries_raw_file) %>% 
    as_tibble() %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  save(ots_countries, file = countries_tidy_file, version = 2)
}


# Product codes

products_url <- paste0(base_url, "products")
products_raw_file <- "data-raw/ots_products.json"
products_tidy_file <- "data/ots_products.rda"

if (!file.exists(products_raw_file)) { download.file(products_url, products_raw_file) }

if (!file.exists(products_tidy_file)) {
  ots_products <- fromJSON(products_raw_file) %>% 
    as_tibble() %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  save(ots_products, file = products_tidy_file, version = 2)
}


# Community codes

communities_url <- paste0(base_url, "products_communities")
communities_raw_file <- "data-raw/ots_communities.json"
communities_tidy_file <- "data/ots_communities.rda"

if (!file.exists(communities_raw_file)) { download.file(communities_url, communities_raw_file) }

if (!file.exists(communities_tidy_file)) {
  ots_communities <- fromJSON(communities_raw_file) %>% 
    as_tibble() %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  save(ots_communities, file = communities_tidy_file, version = 2)
}

# Product short names

products_shortnames_url <- paste0(base_url, "products_shortnames")
products_shortnames_raw_file <- "data-raw/ots_products_shortnames.json"
products_shortnames_tidy_file <- "data/ots_products_shortnames.rda"

if (!file.exists(products_shortnames_raw_file)) { download.file(products_shortnames_url, products_shortnames_raw_file) }

if (!file.exists(products_shortnames_tidy_file)) {
  ots_products_shortnames <- fromJSON(products_shortnames_raw_file) %>% 
    as_tibble() %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  save(ots_products_shortnames, file = products_shortnames_tidy_file, version = 2)
}

# Conversion rates

# Source
# https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD

inflation_url <- "https://raw.githubusercontent.com/tradestatistics/inflation-data/master/inflation-data.json"
inflation_raw_file <- "data-raw/ots_inflation.json"
inflation_tidy_file <- "data/ots_inflation.rda"

if (!file.exists(inflation_raw_file)) { download.file(inflation_url, inflation_raw_file) }

if (!file.exists(inflation_tidy_file)) {
  ots_inflation <- fromJSON(inflation_url)
  ots_inflation <- as_tibble(ots_inflation)
  save(ots_inflation, file = inflation_tidy_file, version = 2)
}

