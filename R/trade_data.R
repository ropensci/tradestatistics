#' Downloads and processes the data from the API
#' @description This function accesses \code{api.tradestatistics.io} and 
#' performs different API calls to return tidy data. and data transforming.
#' @param years Numeric value greater or equal to 1962 and lower of equal 
#' to 2016. Default set to \code{NULL}.
#' @param reporter ISO code for country of reporter (e.g. \code{chl} for 
#' Chile). Default set to \code{NULL}.
#' Run \code{countries} in case of doubt.
#' @param partner ISO code for country of partner (e.g. \code{chn} for 
#' China). Default set to \code{NULL}.
#' Run \code{countries} in case of doubt.
#' @param commodity_code_length Character string to indicate the granularity level on commodities. 
#' Default set to \code{4} (it can also take the values \code{6} or 
#' \code{all}).
#' @param table Character string to select the table to obtain the data. 
#' Default set to \code{yrpc} (Year - Reporter - Partner - Commodity).
#' Run \code{tables} in case of doubt.
#' @param max_attempts How many times to try to download data in case the 
#' API or the internet connection fails when obtaining data. Default set 
#' to \code{5}.
#' @return A tibble that describes bilateral trade metrics (imports, 
#' exports, trade balance and relevant metrics
#' such as exports growth w/r to last year) between an \code{reporter} 
#' and \code{partner} country.
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble select filter mutate everything
#' everything left_join bind_rows rename matches
#' @importFrom stringr str_sub str_length
#' @importFrom jsonlite fromJSON
#' @importFrom crul HttpClient
#' @importFrom rlang sym syms
#' @importFrom purrr map_df
#' @export
#' @examples
#' \dontrun{
#' # The next example can take more than 5 seconds to compute, 
#' so these are just shown without evaluation according to CRAN rules
#'
#' # Run `countries` to display the full table of countries
#'
#' # What does Chile export to China?
#' trade_data(reporter = "chl", partner = "chn", years = 2015)
#' }
#' @keywords functions

trade_data <- function(years = NULL, 
                     reporter = NULL, 
                     partner = NULL, 
                     commodity_code_length = 4, 
                     table = "yrpc", 
                     max_attempts = 5) {
  # Check years -------------------------------------------------------------
  year_depending_queries <- c("reporters", 
                              "country_rankings", 
                              "product_rankings", 
                              "yrpc", 
                              "yrp", 
                              "yrc", 
                              "yr", 
                              "yc")
  
  if (all(years %in% 1962:2016) != TRUE & 
      table %in% year_depending_queries) {
    stop(
      "Please verify that you are requesting data
       contained within the years 1962-2016."
    )
  }
  
  # Package data ------------------------------------------------------------
  products <- tradestatistics::products
  countries <- tradestatistics::countries
  tables <- tradestatistics::tables
  
  # Check tables ------------------------------------------------------------
  if (!table %in% tables$table) {
    stop(
      "The requested table does not exist. Please check the spelling or 
       explore the tables table provided within this package."
    )
  }
  
  # Check reporter and partner ----------------------------------------------
  reporter_depending_queries <- c("yrpc", "yrp", "yrc", "yr")
  partner_depending_queries <- c("yrpc", "yrp")
  
  if (!is.null(reporter)) {
    if (!reporter %in% countries$country_iso & 
        table %in% reporter_depending_queries
    ) {
      reporter <- country_code(reporter)
      match.arg(reporter, countries$country_iso)
    }
  }
  
  if (!is.null(partner)) {
    if (!partner %in% countries$country_iso & 
        table %in% partner_depending_queries
    ) {
      partner <- country_code(partner)
      match.arg(partner, countries$country_iso)
    }
  }
  
  # Read from API -----------------------------------------------------------
  read_from_api <- function(t, attempts_left = max_attempts) {
    stopifnot(attempts_left > 0)
    
    url <- switch(
      table,
      "countries" = "countries",
      "products" = "products",
      "reporters" = sprintf("reporters?y=%s", years[t]),
      "country_rankings" = sprintf("country_rankings?y=%s", years[t]),
      "product_rankings" = sprintf("product_rankings?y=%s", years[t]),
      "yrpc" = sprintf("yrpc?y=%s&r=%s&p=%s&l=%s", 
                       years[t], reporter, partner, commodity_code_length),
      "yrp" = sprintf("yrp?y=%s&r=%s&p=%s", years[t], reporter, partner),
      "yrc" = sprintf("yrc?y=%s&r=%s&l=%s", 
                      years[t], reporter, commodity_code_length),
      "yr" = sprintf("yr?y=%s&r=%s", years[t], reporter),
      "yc" = sprintf("yc?y=%s&l=%s", years[t], commodity_code_length)
    )
    
    resp <- crul::HttpClient$new(url = "https://api.tradestatistics.io/")
    resp <- resp$get(url)
    
    # on a successful GET, return the response
    if (resp$status_code == 200) {
      sprintf("Trying to download data for the year %s...", years[t])
      
      data <- try(
        fromJSON(resp$parse(encoding = "UTF-8"))
      )
      
      if (!is.data.frame(data)) {
        stop("It wasn't possible to obtain data.
             Provided this function tests your internet connection there was 
             a server problem.
             Please try again later.")
      }
      
      sprintf("Data for the year was downloaded without problems.")
      
      return(data)
    } else if (attempts_left == 0) {
      # when attempts run out, stop with an error
      stop(
        "Cannot connect to the API. Either the server is down or there is a 
         connection problem.")
    } else {
      # otherwise, sleep a second and try again
      Sys.sleep(1)
      read_from_api(t, attempts_left = attempts_left - 1)
    }
  }
  
  data <- as_tibble(map_df(seq_along(years), read_from_api))
  
  # no data in API message
  if (nrow(data) == 0) {
    stop("No data available. Try changing years or trade classification.")
  }
  
  # Add attributes based on codes, etc (and join years, if applicable) ------
  
  # include countries data
  tables_with_reporter <- c("yrpc", "yrp", "yrc", "yr")
  
  if (table %in% tables_with_reporter) {
    if (table %in% tables_with_reporter[1:2]) {
    data <- data %>%
      left_join(select(countries, 
                       !!!syms(
                         c("country_iso", "country_fullname_english")
                      )), 
                by = c("reporter_iso" = "country_iso")) %>%
      rename(
        reporter_fullname_english = !!sym("country_fullname_english")
      ) %>% 
      select(
        !!!syms(c(
          "year",
          "reporter_iso",
          "partner_iso",
          "reporter_fullname_english"
        )),
        everything()
      )
    } else {
      data <- data %>%
        left_join(select(countries, 
                         !!!syms(
                           c("country_iso", "country_fullname_english")
                        )), 
                  by = c("reporter_iso" = "country_iso")) %>%
        rename(
          reporter_fullname_english = !!sym("country_fullname_english")
        ) %>% 
        select(
          !!!syms(c(
            "year",
            "reporter_iso",
            "reporter_fullname_english"
          )),
          everything()
        )
    }
  }
  
  tables_with_partner <- c("yrpc", "yrp")
  
  if (table %in% tables_with_partner) {
    data <- data %>% 
      left_join(select(countries, 
                       !!!syms(
                         c("country_iso", "country_fullname_english")
                      )), 
                by = c("partner_iso" = "country_iso")) %>%
      rename(
        partner_fullname_english = !!sym("country_fullname_english")
      ) %>% 
      select(
        !!!syms(c(
          "year",
          "reporter_iso",
          "partner_iso",
          "reporter_fullname_english",
          "partner_fullname_english"
        )),
        everything()
      )
  }
  
  # include products data
  tables_with_commodity_code <- c("yrpc", "yrc", "yc")
  
  if (table %in% tables_with_commodity_code) {
    data <- data %>% 
      left_join(products, by = "commodity_code")
    
    if (table == "yrpc") {
      data <- data %>% 
        select(
          !!!syms(c(
            "year",
            "reporter_iso",
            "partner_iso",
            "reporter_fullname_english",
            "partner_fullname_english",
            "commodity_code",
            "commodity_code_length",
            "product_fullname_english",
            "group_code",
            "group_name"
          )),
          everything()
        )
    }
    
    if (table == "yrc") {
      data <- data %>% 
        select(
          !!!syms(c(
            "year",
            "reporter_iso",
            "reporter_fullname_english",
            "commodity_code",
            "commodity_code_length",
            "product_fullname_english",
            "group_code",
            "group_name"
          )),
          everything()
        )
    }
    
    if (table == "yc") {
      data <- data %>% 
        select(
          !!!syms(c(
            "year",
            "commodity_code",
            "commodity_code_length",
            "product_fullname_english",
            "group_code",
            "group_name"
          )),
          everything()
        )
    }
  }
  
  return(data)
}