# Tables ----

library(data.table)
library(dplyr)
library(tidyr)
library(jsonlite)
library(arrow)
library(scales)
library(stringr)
library(tradestatistics)

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

# Country codes ----

countries_url <- paste0(base_url, "countries")
countries_raw_file <- "data-raw/ots_countries.json"
countries_tidy_file <- "data/ots_countries.rda"

if (!file.exists(countries_raw_file)) { download.file(countries_url, countries_raw_file) }

if (!file.exists(countries_tidy_file)) {
  ots_countries <- fromJSON(countries_raw_file) %>% 
    as.data.table() %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  
  ots_countries <- ots_countries %>% 
    arrange(continent_id) %>% 
    mutate_if(is.numeric, as.integer)
  
  countries_in_data <- open_dataset("../hs12-visualization/yrp",
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

# Commodity codes ----

commodities_url <- paste0(base_url, "commodities")
commodities_raw_file <- "data-raw/ots_commodities.json"
commodities_tidy_file <- "data/ots_commodities.rda"

if (!file.exists(commodities_raw_file)) { download.file(commodities_url, commodities_raw_file) }

if (!file.exists(commodities_tidy_file)) {
  ots_commodities <- fromJSON(commodities_raw_file) %>% 
    as.data.table() %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  
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
  
  ots_commodities <- ots_commodities %>% 
    mutate(
      section_code = ifelse(is.na(section_code), "999", section_code)
    )
  
  ots_commodities <- ots_commodities %>% 
    select(-c(group_name, group_fullname_english))
  
  save(ots_commodities, file = commodities_tidy_file, version = 2)
}

# Sections codes ----

sections_url <- paste0(base_url, "sections")
sections_raw_file <- "data-raw/ots_sections.json"
sections_tidy_file <- "data/ots_sections.rda"

if (!file.exists(sections_raw_file)) { download.file(sections_url, sections_raw_file) }

if (!file.exists(sections_tidy_file)) {
  ots_sections <- fromJSON(sections_raw_file) %>% 
    as.data.table() %>% 
    mutate_if(is.character, function(x) { iconv(x, to = "ASCII//TRANSLIT")})
  
  save(ots_sections, file = sections_tidy_file, version = 2)
}

# Shorter commodity codes ----

load("../comtrade-codes/02-2-tidy-product-data/product-codes.RData")

ots_commodities_short <- product_codes %>%
  filter(
    classification == "H4",
    str_length(code) == 4
  ) %>%
  select(commodity_code = code, commodity_fullname_english = description)

ots_commodities_short <- as.data.table(ots_commodities_short)

save(ots_commodities_short, file = "data/ots_commodities_short.rda", version = 2)

# Conversion rates ----

# Source
# https://data.worldbank.org/indicator/FP.CPI.TOTL.ZG
# https://data.worldbank.org/indicator/NY.GDP.MKTP.KD

inflation_json <- "../inflation-data/inflation-data.json"
inflation_raw_file <- "data-raw/ots_inflation.json"
inflation_tidy_file <- "data/ots_inflation.rda"

if (!file.exists(inflation_tidy_file)) {
  ots_inflation <- fromJSON(inflation_json)
  ots_inflation <- as.data.table(ots_inflation) %>% 
    filter(from >= 1976)
  save(ots_inflation, file = inflation_tidy_file, version = 2)
}

# Colors ----

# jan 15: add colours
ots_sections_colors <- readr::read_csv("https://raw.githubusercontent.com/tradestatistics/visualization-with-shiny/master/sections_colors.csv")

ots_sections_colors <- ots_sections_colors %>% 
  left_join(
    tradestatistics::ots_commodities %>% 
      select(section_fullname_english, section_code) %>% 
      distinct()
  ) %>% 
  select(section_code, section_color) %>% 
  as.data.table()

ots_sections_colors <- ots_sections_colors %>% 
  filter(!is.na(section_code))

ots_sections_colors <- ots_sections_colors %>% 
  bind_rows(
    tibble(section_code = "999", section_color = "#d3d3d3")
  ) %>% 
  as.data.table()

save(ots_sections_colors, file = "data/ots_sections_colors.rda", version = 2)

####

ots_countries_colors <- tradestatistics::ots_countries %>%
  select(country_iso, country_name_english, continent_id)
 
# cols <- unique(tradestatistics::ots_sections_colors$section_color)
# 
# show_col(cols)
# 
# set.seed(1234)
# 
# colsk <- kmeans(t(col2rgb(cols)), 8)
# 
# set.seed(4321)
# 
# cols2 <- tibble(
#   color = cols,
#   center = colsk$cluster
# ) %>% 
#   group_by(center) %>% 
#   sample_n(1)

cols2 <- tibble(
  color = c("#406662", "#ede788", "#7454a6", "#5c57d9", "#d05555", "#dc8e7a", "#bcd8af", "#d1a1bc"),
  center = 1:8
)

show_col(cols2$color)

ots_countries_colors <- ots_countries_colors %>% 
  filter(!grepl("all|c-", country_iso))

ots_countries_colors <- ots_countries_colors %>% 
  mutate(
    continent_id = case_when(
      as.character(str_match(country_name_english, "Asia")) == "Asia" ~ "1",
      as.character(str_match(country_name_english, "Europe")) == "Europe" ~ "2",
      as.character(str_match(country_name_english, "Africa")) == "Africa" ~ "3",
      as.character(str_match(country_name_english, "Oceania")) == "Oceania" ~ "4",
      as.character(str_match(country_name_english, "America")) == "America" ~ "5",
      as.character(str_match(country_name_english, "Caribbean")) == "Caribbean" ~ "5",
      as.character(str_match(country_name_english, "Antarctic")) == "Antarctic" ~ "6",
      TRUE ~ as.character(continent_id)
    )
  ) %>% 
  mutate(continent_id = as.integer(continent_id)) %>% 
  left_join(cols2, by = c("continent_id" = "center")) %>% 
  mutate(
    color = case_when(
      country_name_english == "United States Miscellaneous Pacific Islands" ~ "#d05555",
      country_name_english == "Neutral zone" ~ "#bcd8af",
      country_name_english == "Free zones" ~ "#bcd8af",
      country_name_english == "Bunkers" ~ "#bcd8af",
      country_name_english == "Special categories" ~ "#bcd8af",
      country_name_english == "Areas, not elsewhere specified" ~ "#d1a1bc",
      TRUE ~ color
    )
  )

# ots_countries_colors <- ots_countries_colors %>% 
#   group_by(continent_id, country_name_english) %>% 
#   arrange(color) %>% 
#   group_by(continent_id) %>% 
#   mutate(
#     n = row_number(),
#     white = n / max(n)
#   )

# max(ots_countries_colors$white) <= 1
# min(ots_countries_colors$white) >= 0
# 
# col1 <- (1 - (ots_countries_colors$white / 4)) * t(col2rgb(ots_countries_colors$color))
# col2 <- (ots_countries_colors$white / 4) * t(col2rgb(rep("#ffffff", nrow(col1))))
# col <- col1 + col2
# col <- rgb(col[,1], col[,2], col[,3], maxColorValue = 255)

# show_col(col)

ots_countries_colors <- ots_countries_colors %>% 
  ungroup() %>% 
  select(country_iso, color) %>% 
  rename(country_color = color) %>% 
  as.data.table()

ots_countries_colors <- ots_countries_colors %>% 
  left_join(
    ots_countries %>% 
      select(country_iso, continent_id)
  )

ots_countries_colors_2 <- tibble(
  country_iso = paste0("c-", c("as", "eu", "af", "oc", "am")),
  country_color = cols2$color[1:5],
  continent_id = 1:5
)

ots_countries_colors <- ots_countries_colors %>% 
  bind_rows(ots_countries_colors_2) %>% 
  arrange(country_iso) %>% 
  mutate(country_color = toupper(country_color))

ots_countries_colors <- ots_countries_colors %>% 
  select(continent_id, country_iso, country_color)

save(ots_countries_colors, file = "data/ots_countries_colors.rda", version = 2)
