# Tables

library(dplyr)
library(data.table)
library(jsonlite)

tables_url <- "https://api.tradestatistics.io/tables"
tables_raw_file <- "data-raw/ots_tables.json"
tables_tidy_file <- "data/ots_tables.rda"

if (!file.exists(tables_raw_file)) { download.file(tables_url, tables_raw_file) }

if (!file.exists(tables_tidy_file)) {
  ots_tables <- fromJSON(tables_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  save(ots_tables, file = tables_tidy_file, version = 2)
}


# Country codes

countries_url <- "https://api.tradestatistics.io/countries"
countries_raw_file <- "data-raw/ots_countries.json"
countries_tidy_file <- "data/ots_countries.rda"

if (!file.exists(countries_raw_file)) { download.file(countries_url, countries_raw_file) }

if (!file.exists(countries_tidy_file)) {
  ots_countries <- fromJSON(countries_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  save(ots_countries, file = countries_tidy_file, version = 2)
}


# Product codes

products_url <- "https://api.tradestatistics.io/products"
products_raw_file <- "data-raw/ots_products.json"
products_tidy_file <- "data/ots_products.rda"

if (!file.exists(products_raw_file)) { download.file(products_url, products_raw_file) }

if (!file.exists(products_tidy_file)) {
  ots_products <- fromJSON(products_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    filter(nchar(product_code) %in% c(2,3,4)) %>% 
    as.data.table()
  save(ots_products, file = products_tidy_file, version = 2)
}


# Section codes

sections_url <- "https://api.tradestatistics.io/sections"
sections_raw_file <- "data-raw/ots_sections.json"
sections_tidy_file <- "data/ots_sections.rda"

if (!file.exists(sections_raw_file)) { download.file(sections_url, sections_raw_file) }

if (!file.exists(sections_tidy_file)) {
  ots_sections <- fromJSON(sections_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    filter(nchar(product_code) == 4) %>% 
    as.data.table()
  save(ots_sections, file = sections_tidy_file, version = 2)
}


# Groups codes

groups_url <- "https://api.tradestatistics.io/groups"
groups_raw_file <- "data-raw/ots_groups.json"
groups_tidy_file <- "data/ots_groups.rda"

if (!file.exists(groups_raw_file)) { download.file(groups_url, groups_raw_file) }

if (!file.exists(groups_tidy_file)) {
  ots_groups <- fromJSON(groups_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  save(ots_groups, file = groups_tidy_file, version = 2)
}


# Product short names

products_shortnames_url <- "https://api.tradestatistics.io/products_shortnames"
products_shortnames_raw_file <- "data-raw/ots_products_shortnames.json"
products_shortnames_tidy_file <- "data/ots_products_shortnames.rda"

if (!file.exists(products_shortnames_raw_file)) { download.file(products_shortnames_url, products_shortnames_raw_file) }

if (!file.exists(products_shortnames_tidy_file)) {
  ots_products_shortnames <- fromJSON(products_shortnames_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  save(ots_products_shortnames, file = products_shortnames_tidy_file, version = 2)
}


# Section names

sections_names_url <- "https://api.tradestatistics.io/sections_names"
sections_names_raw_file <- "data-raw/ots_sections_names.json"
sections_names_tidy_file <- "data/ots_sections_names.rda"

if (!file.exists(sections_names_raw_file)) { download.file(sections_names_url, sections_names_raw_file) }

if (!file.exists(sections_names_tidy_file)) {
  ots_sections_names <- fromJSON(sections_names_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  save(ots_sections_names, file = sections_names_tidy_file, version = 2)
}


# Section short names

sections_shortnames_url <- "https://api.tradestatistics.io/sections_shortnames"
sections_shortnames_raw_file <- "data-raw/ots_sections_shortnames.json"
sections_shortnames_tidy_file <- "data/ots_sections_shortnames.rda"

if (!file.exists(sections_shortnames_raw_file)) { download.file(sections_shortnames_url, sections_shortnames_raw_file) }

if (!file.exists(sections_shortnames_tidy_file)) {
  ots_sections_shortnames <- fromJSON(sections_shortnames_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  save(ots_sections_shortnames, file = sections_shortnames_tidy_file,, version = 2)
}


# Section colors

sections_colors_url <- "https://api.tradestatistics.io/sections_colors"
sections_colors_raw_file <- "data-raw/ots_sections_colors.json"
sections_colors_tidy_file <- "data/ots_sections_colors.rda"

if (!file.exists(sections_colors_raw_file)) { download.file(sections_colors_url, sections_colors_raw_file) }

if (!file.exists(sections_colors_tidy_file)) {
  ots_sections_colors <- fromJSON(sections_colors_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  save(ots_sections_colors, file = sections_colors_tidy_file, version = 2)
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
  ots_inflation <- as.data.table(ots_inflation)
  save(ots_inflation, file = inflation_tidy_file, version = 2)
}

