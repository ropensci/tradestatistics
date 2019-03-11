#' Downloads and processes the data from the API to return a human-readable tibble
#' @description This function accesses \code{api.tradestatistics.io} and
#' performs different API calls to return tidy data. and data transforming.
#' @param year Numeric value greater or equal to 1962 and lower of equal
#' to 2016. Default set to \code{NULL}.
#' @param reporter ISO code for country of reporter (e.g. \code{chl} for
#' Chile). Default set to \code{NULL}.
#' Run \code{countries} in case of doubt.
#' @param partner ISO code for country of partner (e.g. \code{chn} for
#' China). Default set to \code{NULL}.
#' Run \code{countries} in case of doubt.
#' @param product Character string (e.g. \code{0101}, \code{01} or \code{"apple"}) to filter products.
#' Default set to \code{"all"}.
#' @param product_code_length Character string to indicate the granularity level on products
#' Default set to \code{4} (it can also take the values \code{6} or
#' \code{all}).
#' @param table Character string to select the table to obtain the data.
#' Default set to \code{yrpc} (Year - Reporter - Partner - Product Code).
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
#' everything left_join bind_rows rename matches distinct
#' @importFrom stringr str_sub str_length
#' @importFrom rlang sym syms
#' @importFrom purrr map2_df as_vector
#' @importFrom jsonlite fromJSON
#' @importFrom crul HttpClient
#' @export
#' @examples
#' \dontrun{
#' # The next example can take more than 5 seconds to compute,
#' # so these are just shown without evaluation according to CRAN rules
#' 
#' # Run `ots_attributes_countries` to display the full table of countries
#' # Run `ots_attributes_products` to display the full table of products
#' 
#' # What does Chile export to China? (1980)
#' ots_create_tidy_data(year = 1980, reporter = "chl", partner = "chn")
#' 
#' # What can we say about Horses export in Chile and the World? (1980)
#' ots_create_tidy_data(year = 1980, product = "0101", table = "yc")
#' ots_create_tidy_data(year = 1980, reporter = "chl", product = "0101", table = "yrc")
#' 
#' # What can we say about the different types of apples exported by Chile? (1980)
#' ots_create_tidy_data(year = 1980, reporter = "chl", product = "apple", table = "yrc")
#' }
#' @keywords functions

ots_create_tidy_data <- function(year = NULL,
                                 reporter = NULL,
                                 partner = NULL,
                                 product = "all",
                                 product_code_length = 4,
                                 table = "yrpc",
                                 max_attempts = 5) {

  # Package data (part 1) ---------------------------------------------------
  tables <- tradestatistics::ots_attributes_tables

  # Check tables ------------------------------------------------------------
  if (!table %in% tables$table) {
    stop(
      "
      The requested table does not exist. Please check the spelling or 
      explore the 'ots_attributes_table' table provided within this package.
      "
    )
  }

  # Check year --------------------------------------------------------------
  year_depending_queries <- grep("^reporters|rankings$|^y", tables$table, value = T)

  if (all(year %in% 1962:2016) != TRUE &
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
        table %in% reporter_depending_queries) {
        reporter <- tradestatistics::ots_country_code(reporter)
        match.arg(reporter, countries$country_iso)
    }
  }
  
  if (!is.null(partner)) {
    if (!partner %in% countries$country_iso &
        table %in% partner_depending_queries) {
        partner <- tradestatistics::ots_country_code(partner)
        match.arg(partner, countries$country_iso)
    }
  }
  
  # Check product code ------------------------------------------------------
  product_depending_queries <- grep("c$", tables$table, value = T)
  
  if(!as.character(product) %in% products$product_code &
     table %in% product_depending_queries) {
      # product name match (pmm)
      pnm <- tradestatistics::ots_product_code(productname = product)
      
      # group name match (gnm)
      gnm <- tradestatistics::ots_product_code(productgroup = product)
      
      product <- bind_rows(pnm, gnm) %>% 
        distinct(!!sym("product_code")) %>% 
        as_vector()
  }

  if(!all(as.character(product) %in% products$product_code == TRUE) &
     table %in% product_depending_queries) {
      stop(
        "
        The requested product does not exist. Please check the spelling or 
        explore the 'ots_attributes_table' table provided within this package.
        "
      )
  }

  # Read from API -----------------------------------------------------------
  if (!table %in% product_depending_queries & any(product != "all") == TRUE) {
    product <- "all"
    message(
      "
      The product code argument will be ignored provided that you requested a table
      without product code field.
      "
    )
  }
  
  data <- dplyr::as_tibble(
    purrr::map2_df(.x = seq_along(year),
                   .y = seq_along(product),
                   ~ots_read_from_api(
                     table = table,
                     max_attempts = max_attempts,
                     year = year[.x],
                     reporter = reporter,
                     partner = partner,
                     product_code = product[.y],
                     product_code_length = product_code_length
                   )
    ) %>% 
    dplyr::filter(!is.na(product_code_length))
  )

  # no data in API message
  if (nrow(data) == 0) {
    stop("No data available. Try changing year, reporter, partner or product")
  }

  # Add attributes based on codes, etc (and join year, if applicable) -------

  # include countries data
  tables_with_reporter <- c("yrpc", "yrp", "yrc", "yr")

  if (table %in% tables_with_reporter) {
    if (table %in% tables_with_reporter[1:2]) {
      data <- data %>%
        dplyr::left_join(dplyr::select(
          countries,
          !!!rlang::syms(
            c("country_iso", "country_fullname_english")
          )
        ),
        by = c("reporter_iso" = "country_iso")
        ) %>%
        dplyr::rename(
          reporter_fullname_english = !!rlang::sym("country_fullname_english")
        ) %>%
        dplyr::select(
          !!!rlang::syms(c(
            "year",
            "reporter_iso",
            "partner_iso",
            "reporter_fullname_english"
          )),
          dplyr::everything()
        )
    } else {
      data <- data %>%
        dplyr::left_join(dplyr::select(
          countries,
          !!!rlang::syms(
            c("country_iso", "country_fullname_english")
          )
        ),
        by = c("reporter_iso" = "country_iso")
        ) %>%
        dplyr::rename(
          reporter_fullname_english = !!rlang::sym("country_fullname_english")
        ) %>%
        dplyr::select(
          !!!rlang::syms(c(
            "year",
            "reporter_iso",
            "reporter_fullname_english"
          )),
          dplyr::everything()
        )
    }
  }

  tables_with_partner <- c("yrpc", "yrp")

  if (table %in% tables_with_partner) {
    data <- data %>%
      dplyr::left_join(dplyr::select(
        countries,
        !!!rlang::syms(
          c("country_iso", "country_fullname_english")
        )
      ),
      by = c("partner_iso" = "country_iso")
      ) %>%
      dplyr::rename(
        partner_fullname_english = !!rlang::sym("country_fullname_english")
      ) %>%
      dplyr::select(
        !!!rlang::syms(c(
          "year",
          "reporter_iso",
          "partner_iso",
          "reporter_fullname_english",
          "partner_fullname_english"
        )),
        dplyr::everything()
      )
  }

  # include products data
  tables_with_product_code <- c("yrpc", "yrc", "yc")

  if (table %in% tables_with_product_code) {
    data <- data %>%
      dplyr::left_join(products, by = "product_code")

    if (table == "yrpc") {
      data <- data %>%
        dplyr::select(
          !!!rlang::syms(c(
            "year",
            "reporter_iso",
            "partner_iso",
            "reporter_fullname_english",
            "partner_fullname_english",
            "product_code",
            "product_code_length",
            "product_fullname_english",
            "group_code",
            "group_name"
          )),
          dplyr::everything()
        )
    }

    if (table == "yrc") {
      data <- data %>%
        dplyr::select(
          !!!rlang::syms(c(
            "year",
            "reporter_iso",
            "reporter_fullname_english",
            "product_code",
            "product_code_length",
            "product_fullname_english",
            "group_code",
            "group_name"
          )),
          dplyr::everything()
        )
    }

    if (table == "yc") {
      data <- data %>%
        dplyr::select(
          !!!rlang::syms(c(
            "year",
            "product_code",
            "product_code_length",
            "product_fullname_english",
            "group_code",
            "group_name"
          )),
          dplyr::everything()
        )
    }
  }

  return(data)
}
