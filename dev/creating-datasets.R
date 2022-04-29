# Tables ----

library(data.table)
library(dplyr)
library(arrow)

base_url <- "https://api.tradestatistics.io/"
# base_url <- "http://127.0.0.1:8080/"
  
tables_url <- paste0(base_url, "tables")
tables_raw_file <- "data-raw/ots_tables.csv"
tables_tidy_file <- "data/ots_tables.rda"

if (!file.exists(tables_raw_file)) { download.file(tables_url, tables_raw_file) }

if (!file.exists(tables_tidy_file)) {
  ots_tables <- fread(tables_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  save(ots_tables, file = tables_tidy_file, version = 2)
}

# Country codes ----

countries_url <- paste0(base_url, "countries")
countries_raw_file <- "data-raw/ots_countries.parquet"
countries_tidy_file <- "data/ots_countries.rda"

if (!file.exists(countries_raw_file)) { download.file(countries_url, countries_raw_file) }

if (!file.exists(countries_tidy_file)) {
  ots_countries <- read_parquet(countries_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    mutate_if(is.numeric, as.integer) %>% 
    as.data.table()
  
  save(ots_countries, file = countries_tidy_file, version = 2)
}

# Commodity codes ----

commodities_url <- paste0(base_url, "commodities")
commodities_raw_file <- "data-raw/ots_commodities.parquet"
commodities_tidy_file <- "data/ots_commodities.rda"

if (!file.exists(commodities_raw_file)) { download.file(commodities_url, commodities_raw_file) }

if (!file.exists(commodities_tidy_file)) {
  ots_commodities <- read_parquet(commodities_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  
  save(ots_commodities, file = commodities_tidy_file, version = 2)
}

# Sections codes ----

sections_url <- paste0(base_url, "sections")
sections_raw_file <- "data-raw/ots_sections.parquet"
sections_tidy_file <- "data/ots_sections.rda"

if (!file.exists(sections_raw_file)) { download.file(sections_url, sections_raw_file) }

if (!file.exists(sections_tidy_file)) {
  ots_sections <- read_parquet(sections_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  
  save(ots_sections, file = sections_tidy_file, version = 2)
}

# Shorter commodity codes ----

commodities_short_url <- paste0(base_url, "commodities_short")
commodities_short_raw_file <- "data-raw/ots_commodities_short.parquet"
commodities_short_tidy_file <- "data/ots_commodities_short.rda"

if (!file.exists(commodities_short_raw_file)) { download.file(commodities_short_url, commodities_short_raw_file) }

if (!file.exists(commodities_short_tidy_file)) {
  ots_commodities_short <- read_parquet(commodities_short_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  
  save(ots_commodities_short, file = commodities_short_tidy_file, version = 2)
}

# GDP deflator ----

# Source
# https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG

gdp_deflator_csv <- "../gdp-deflator-data/gdp-deflator-data.csv"
gdp_deflator_tidy_file <- "data/ots_gdp_deflator.rda"

if (!file.exists(gdp_deflator_tidy_file)) {
  ots_gdp_deflator <- fread(gdp_deflator_csv)
  ots_gdp_deflator <- ots_gdp_deflator %>% 
    filter(from >= 2000) %>% 
    filter(to <= 2020) %>% 
    mutate(from = as.integer(from), to = as.integer(to))

  save(ots_gdp_deflator, file = gdp_deflator_tidy_file, version = 2)
}

# Colors ----

sections_colors_url <- paste0(base_url, "sections_colors")
sections_colors_raw_file <- "data-raw/ots_sections_colors.parquet"
sections_colors_tidy_file <- "data/ots_sections_colors.rda"

if (!file.exists(sections_colors_raw_file)) { download.file(sections_colors_url, sections_colors_raw_file) }

if (!file.exists(sections_colors_tidy_file)) {
  ots_sections_colors <- read_parquet(sections_colors_raw_file) %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")}) %>% 
    as.data.table()
  
  save(ots_sections_colors, file = sections_colors_tidy_file, version = 2)
}
