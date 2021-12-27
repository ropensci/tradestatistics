# Tables

library(data.table)
library(dplyr)
library(jsonlite)
library(arrow)

# base_url <- "https://api.tradestatistics.io/"
base_url <- "http://127.0.0.1:8080/"
  
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
        country_iso = c("ata", "bes", "blm", "cuw", "sxm", "ssd"),
        country_name_english = c("Antarctica", "Bonaire, Sint Eustatius and Saba", 
                                 "Saint Barthelemy", "Curacao", "Sint Maarten", "South Sudan"),
        country_fullname_english = c("Antarctica", "Bonaire, Sint Eustatius and Saba", 
                                     "Saint Barthelemy", "Curacao", "Sint Maarten", "South Sudan"),
        continent_id = c(6, 5, 5, 5, 5, 3),
        continent = c("Antarctica", "Americas", "Americas", "Americas", "Americas", "Africa"),
        eu28_member = c(0, 0, 0, 0, 0, 0)
      )) %>% 
    arrange(country_iso)
  
  ots_countries <- ots_countries %>% 
    arrange(continent_id) %>% 
    mutate_if(is.numeric, as.integer)
  
  countries_in_data <- open_dataset("../hs92-historic-series/hs92-visualization/yrp",
                                    partitioning = c("year", "reporter_iso"))
  
  countries_in_data <- countries_in_data %>% 
    select(country_iso = reporter_iso) %>% 
    collect() %>% 
    distinct() %>% 
    bind_rows(
      countries_in_data %>% 
        select(country_iso = partner_iso) %>% 
        collect() %>% 
        distinct()
    ) %>% 
    mutate_if(is.character, function(x) gsub(".*=", "", x)) %>% 
    distinct()
  
  ots_countries %>% anti_join(countries_in_data)
  countries_in_data %>% anti_join(ots_countries)
  
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
  
  # update dec 21: add sections
  # this was added to API so we don't need to run this twice
  library(readxl)
  library(stringr)
  library(tidyr)
  
  hs_sections <- readxl::read_excel("data-raw/hs_sections.xlsx")
  
  hs_sections <- hs_sections %>% 
    mutate(section_name = str_to_sentence(str_trim(section_name)))
  
  hs_sections <- hs_sections %>% 
    separate("groups", c("gmin", "gmax")) %>% 
    mutate(gmax = ifelse(is.na(gmax), gmin, gmax))
  
  hs_sections <- hs_sections %>% 
    rowwise() %>% 
    mutate(g = paste(seq(gmin, gmax), collapse = ",")) %>% 
    ungroup() %>% 
    select(-c(gmin,gmax)) %>% 
    separate(g, paste0("g",1:20)) %>% 
    pivot_longer(g1:g20) %>% 
    drop_na() %>% 
    mutate(value = str_pad(value, 2, "left", "0")) %>% 
    select(-name) %>% 
    rename(
      group_code = value,
      section_fullname_english = section_name
    )
  
  ots_commodities <- ots_commodities %>% 
    left_join(hs_sections)
  
  save(ots_commodities, file = commodities_tidy_file, version = 2)
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
  ots_inflation <- as.data.table(ots_inflation) %>% 
    filter(from >= 2000)
  save(ots_inflation, file = inflation_tidy_file, version = 2)
}
