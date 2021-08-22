# Tables

library(data.table)
library(dplyr)
library(jsonlite)

base_url <- "https://api.tradestatistics.io/"
# base_url <- "http://127.0.0.1:8080/"
  
tables_url <- paste0(base_url, "tables")
tables_raw_file <- "data-raw/ots_tables.json"
tables_tidy_file <- "data/ots_tables.rda"

if (!file.exists(tables_raw_file)) { download.file(tables_url, tables_raw_file) }

if (!file.exists(tables_tidy_file)) {
  ots_tables <- fromJSON(tables_raw_file) %>% 
    as.data.table() %>% 
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
    as.data.table() %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  
  # fix Aruba, Roumania and Timor-Leste ISO codes
  # and add countries present in UN COMTRADE kast data
  # Antarctica, Saint Barthelemy, Curacao, Sint Maarten and South Sudan
  ots_countries <- ots_countries %>% 
    mutate(
      country_iso = case_when(
        country_iso == "arb" ~ "abw",
        country_iso == "rom" ~ "rou",
        country_iso == "tmp" ~ "tls",
        TRUE ~ country_iso
      )
    ) %>% 
    bind_rows(
      data.table(
        country_iso = c("ata", "blm", "cuw", "sxm", "ssd"),
        country_name_english = c("Antarctica", "Saint Barthelemy", "Curacao", "Sint Maarten", "South Sudan"),
        country_fullname_english = c("Antarctica", "Saint Barthelemy", "Curacao", "Sint Maarten", "South Sudan"),
        continent_id = c(6, 5, 5, 5, 3),
        continent = c("Antarctica", "Americas", "Americas", "Americas", "Africa"),
        eu28_member = c(0, 0, 0, 0, 0)
      )) %>% 
    arrange(country_iso)
  
  ots_countries <- ots_countries %>% 
    arrange(continent_id) %>% 
    mutate_if(is.numeric, as.integer)
  
  save(ots_countries, file = countries_tidy_file, version = 2)
}


# Commodity codes

commodities_url <- paste0(base_url, "commodities")
commodities_raw_file <- "data-raw/ots_commodities.json"
commodities_tidy_file <- "data/ots_commodities.rda"

if (!file.exists(commodities_raw_file)) { download.file(commodities_url, commodities_raw_file) }

if (!file.exists(commodities_tidy_file)) {
  ots_commodities <- fromJSON(commodities_raw_file) %>% 
    as.data.table() %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  save(ots_commodities, file = commodities_tidy_file, version = 2)
}


# Community codes

communities_url <- paste0(base_url, "commodities_communities")
communities_raw_file <- "data-raw/ots_communities.json"
communities_tidy_file <- "data/ots_communities.rda"

if (!file.exists(communities_raw_file)) { download.file(communities_url, communities_raw_file) }

if (!file.exists(communities_tidy_file)) {
  ots_communities <- fromJSON(communities_raw_file) %>% 
    as.data.table() %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  save(ots_communities, file = communities_tidy_file, version = 2)
}

# Commodities short names

commodities_shortnames_url <- paste0(base_url, "commodities_shortnames")
commodities_shortnames_raw_file <- "data-raw/ots_commodities_shortnames.json"
commodities_shortnames_tidy_file <- "data/ots_commodities_shortnames.rda"

if (!file.exists(commodities_shortnames_raw_file)) { download.file(commodities_shortnames_url, commodities_shortnames_raw_file) }

if (!file.exists(commodities_shortnames_tidy_file)) {
  ots_commodities_shortnames <- fromJSON(commodities_shortnames_raw_file) %>% 
    as.data.table() %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  save(ots_commodities_shortnames, file = commodities_shortnames_tidy_file, version = 2)
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

