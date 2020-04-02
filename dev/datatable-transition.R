library(dplyr)
library(dtplyr)

# ots_strings_processing ----

productname = "sheep"
productgroup = "animal"
productsection = "animal"
countryname = "chile"

d <- lazy_dt(tradestatistics::ots_countries)
d %>% 
  filter(
    grepl(
      countryname, tolower(!!sym("country_fullname_english"))
    )
  )

# ots_inflation_adjustment ----

year = 2010
reference_year = 2000

d <- lazy_dt(tradestatistics::ots_inflation)

d %>% 
  filter(
    !!sym("from") >= reference_year,
    !!sym("from") < year
  ) %>%
  summarise(
    conversion_factor = 1 / last(cumprod(!!sym("conversion_factor")))
  ) %>%
  mutate(
    year = year,
    conversion_year = reference_year
  ) %>%
  select(!!!syms(c("year", "conversion_year", "conversion_factor")))

d <- lazy_dt(ots_create_tidy_data(2010,"chl","arg", table = "yr"))

d %>% 
  mutate_if(is.numeric)
