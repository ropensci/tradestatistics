#' Reads data from the API (internal function)
#' @description This function accesses \code{api.tradestatistics.io} and
#' performs different API calls to return \code{data.frames} by reading \code{JSON} data
#' @param year Year contained within the years specified in
#' api.tradestatistics.io/year_range (e.g. \code{1980}).
#' Default set to \code{NULL}.
#' @param reporter ISO code for reporter country (e.g. \code{"chl"}). Default set to \code{NULL}.
#' @param partner ISO code for partner country (e.g. \code{"chl"}). Default set to \code{NULL}.
#' @param product_code HS code (e.g. \code{0101} or \code{01}) to filter products.
#' Default set to \code{"all"}.
#' @param product_code_length Integer to indicate the granularity level on products.
#' Default set to \code{4}.
#' @param table Character string to select the table to obtain the data. Default set to \code{yrpc}
#' (Year - Reporter - Partner - product).
#' @param max_attempts Number of attempts to retry in case of data retrieving failure.
#' Default set to \code{5}.
#' @param use_localhost Logical to determine if the base URL shall be localhost instead
#' of api.tradestatistics.io. Default set to \code{FALSE}.
#' @importFrom jsonlite fromJSON
#' @importFrom crul HttpClient
#' @examples
#' \dontrun{
#' # The next examples can take more than 5 seconds to compute,
#' # so these are shown without evaluation according to CRAN rules
#'
#' # Run `countries` to display the full table of countries
#'
#' # What does Chile export to China? (1980)
#' ots_read_from_api(year = 1980, reporter = "chl", partner = "chn")
#'
#' # What can we say about chilean Horses export? (1980)
#' ots_read_from_api(year = 1980, product_code = "0101", table = "yc")
#' ots_read_from_api(year = 1980, reporter = "chl", product_code = "0101", table = "yrc")
#' ots_read_from_api(
#'   year = 1980, reporter = "chl", partner = "arg", product_code = "0101",
#'   table = "yrpc"
#' )
#' }
#' @keywords internal

ots_read_from_api <- function(year = NULL,
                              reporter = NULL,
                              partner = NULL,
                              product_code = "all",
                              product_code_length = 4,
                              table = "yrpc",
                              max_attempts = 5,
                              use_localhost = FALSE) {
  stopifnot(max_attempts > 0)

  url <- switch(
    table,
    "countries" = "countries",
    "products" = "products",
    "reporters" = sprintf("reporters?y=%s", year),
    "country_rankings" = sprintf("country_rankings?y=%s", year),
    "product_rankings" = sprintf("product_rankings?y=%s", year),
    "yrpc" = sprintf(
      "yrpc?y=%s&r=%s&p=%s&c=%s&l=%s",
      year, reporter, partner, product_code, product_code_length
    ),
    "yrp" = sprintf("yrp?y=%s&r=%s&p=%s", year, reporter, partner),
    "yrp_short" = sprintf("yrp_short?y=%s&r=%s&p=%s", year, reporter, partner),
    "yrc" = sprintf(
      "yrc?y=%s&r=%s&c=%s&l=%s",
      year, reporter, product_code, product_code_length
    ),
    "yr" = sprintf("yr?y=%s&r=%s", year, reporter),
    "yr_short" = sprintf("yr_short?y=%s&r=%s", year, reporter),
    "yc" = sprintf("yc?y=%s&c=%s&l=%s", year, product_code, product_code_length)
  )

  if (use_localhost == TRUE) {
    base_url <- "http://localhost:8080/"
  } else {
    base_url <- "https://api.tradestatistics.io/"
  }

  resp <- crul::HttpClient$new(url = base_url)
  resp <- resp$get(url)

  # on a successful GET, return the response
  if (resp$status_code == 200) {
    sprintf("Trying to download data for the year %s...", year)

    data <- try(
      jsonlite::fromJSON(resp$parse(encoding = "UTF-8"))
    )

    if (!is.data.frame(data)) {
      stop(
        "
        It wasn't possible to obtain data.
        Provided this function tests your internet connection 
        you misspelled a reporter, partner or table, or there was 
        a server problem.
        Please check and try again.
        "
      )
    }

    sprintf("Data for the year was downloaded without problems.")

    return(data)
  } else if (max_attempts == 0) {
    # when attempts run out, stop with an error
    stop(
      "
        Cannot connect to the API. Either the server is down or there is a 
        connection problem.
      "
    )
  } else {
    # otherwise, sleep a second and try again
    Sys.sleep(1)
    ots_read_from_api(
      year, reporter, partner, product_code_length, table,
      max_attempts = max_attempts - 1
    )
  }
}
