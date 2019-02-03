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
#' @importFrom rlang sym syms
#' @importFrom purrr map_df
#' @export
#' @examples
#' \dontrun{
#' # The next example can take more than 5 seconds to compute,
#' # so these are just shown without evaluation according to CRAN rules
#' 
#' # Run `countries` to display the full table of countries
#' 
#' # What does Chile export to China? (2015)
#' ots_create_tidy_data(years = 2015, reporter = "chl", partner = "chn")
#' }
#' @keywords functions

ots_create_tidy_data <- function(years = NULL,
                                 reporter = NULL,
                                 partner = NULL,
                                 commodity_code_length = 4,
                                 table = "yrpc",
                                 max_attempts = 5) {

  # Package data (part 1) ---------------------------------------------------
  tables <- tradestatistics::ots_attributes_tables

  # Check tables ------------------------------------------------------------
  if (!table %in% tables$table) {
    stop(
      "
      The requested table does not exist. Please check the spelling or 
      explore the 'tables' table provided within this package.
      "
    )
  }

  # Check years -------------------------------------------------------------
  year_depending_queries <- grep("^reporters|rankings$|^y", tables$table, value = T)

  if (all(years %in% 1962:2016) != TRUE &
    table %in% year_depending_queries) {
    stop(
      "
      Provided that the table you requested contains a 'year' field,
      please verify that you are requesting data contained within 
      the years 1962-2016.
      "
    )
  }

  # Package data (part 2) ---------------------------------------------------
  products <- tradestatistics::ots_attributes_products
  countries <- tradestatistics::ots_attributes_countries

  # Check reporter and partner ----------------------------------------------
  reporter_depending_queries <- grep("^yr", tables$table, value = T)
  partner_depending_queries <- grep("^yrp", tables$table, value = T)

  if (!is.null(reporter)) {
    if (!reporter %in% countries$country_iso &
      table %in% reporter_depending_queries
    ) {
      reporter <- ots_country_code(reporter)
      match.arg(reporter, countries$country_iso)
    }
  }

  if (!is.null(partner)) {
    if (!partner %in% countries$country_iso &
      table %in% partner_depending_queries
    ) {
      partner <- ots_country_code(partner)
      match.arg(partner, countries$country_iso)
    }
  }

  # Read from API -----------------------------------------------------------
  data <- as_tibble(
    map_df(
      .x = seq_along(years),
      ~ ots_read_from_api(
        table = table,
        max_attempts = max_attempts,
        years = years[.x],
        reporter = reporter,
        partner = partner,
        commodity_code_length = commodity_code_length
      )
    )
  )

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
        left_join(select(
          countries,
          !!!syms(
            c("country_iso", "country_fullname_english")
          )
        ),
        by = c("reporter_iso" = "country_iso")
        ) %>%
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
        left_join(select(
          countries,
          !!!syms(
            c("country_iso", "country_fullname_english")
          )
        ),
        by = c("reporter_iso" = "country_iso")
        ) %>%
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
      left_join(select(
        countries,
        !!!syms(
          c("country_iso", "country_fullname_english")
        )
      ),
      by = c("partner_iso" = "country_iso")
      ) %>%
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
            "commodity_fullname_english",
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
            "commodity_fullname_english",
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
            "commodity_fullname_english",
            "group_code",
            "group_name"
          )),
          everything()
        )
    }
  }

  return(data)
}
