#' Reads data from the API (internal function)
#' @description Accesses \code{api.tradestatistics.io} and
#' performs different API calls to return \code{data.frames} by reading \code{JSON} data
#' @param year Year contained within the years specified in
#' api.tradestatistics.io/year_range (e.g. \code{1980}).
#' Default set to \code{NULL}.
#' @param reporter_iso ISO code for reporter country (e.g. \code{"chl"}). Default set to \code{"all"}.
#' @param partner_iso ISO code for partner country (e.g. \code{"chl"}). Default set to \code{"all"}.
#' @param commodity_code HS code (e.g. \code{0101} or \code{01}) to filter commodities.
#' Default set to \code{"all"}.
#' @param table Character string to select the table to obtain the data. Default set to \code{yr}
#' (Year - Reporter).
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
#' ots_read_from_api(year = 1980, reporter_iso = "chl", partner_iso = "chn")
#'
#' # What can we say about chilean Horses export? (1980)
#' ots_read_from_api(year = 1980, commodity_code = "0101", table = "yc")
#' ots_read_from_api(year = 1980, reporter_iso = "chl", commodity_code = "0101", table = "yrc")
#' ots_read_from_api(
#'   year = 1980, reporter_iso = "chl", partner_iso = "arg", commodity_code = "0101",
#'   table = "yrpc"
#' )
#' }
#' @keywords internal
ots_read_from_api <- function(year = NULL,
                              reporter_iso = NULL,
                              partner_iso = NULL,
                              commodity_code = "all",
                              table = "yr",
                              max_attempts = 5,
                              use_localhost = FALSE) {
  stopifnot(max_attempts > 0)

  url <- switch(
    table,
    "countries" = "countries",
    "reporters" = sprintf("reporters?y=%s", year),
    "partners" = sprintf("partners?y=%s", year),
    "commodities" = "commodities",
    "yrpc" = sprintf(
      "yrpc?y=%s&r=%s&p=%s&c=%s",
      year, reporter_iso, partner_iso, commodity_code
    ),
    "yrp" = sprintf("yrp?y=%s&r=%s&p=%s", year, reporter_iso, partner_iso),
    "yrc" = sprintf(
      "yrc?y=%s&r=%s&c=%s",
      year, reporter_iso, commodity_code
    ),
    "yr" = sprintf("yr?y=%s&r=%s", year, reporter_iso),
    "yr-groups" = sprintf("yr-groups?y=%s&r=%s", year, reporter_iso),
    "yc" = sprintf("yc?y=%s&c=%s", year, commodity_code),
    "years" = "years"
  )

  if (use_localhost == TRUE) {
    base_url <- "http://localhost:8080/"
  } else {
    base_url <- "https://api.tradestatistics.io/"
  }

  resp <- HttpClient$new(url = base_url)
  resp <- resp$get(url)

  # on a successful GET, return the response
  if (resp$status_code == 200) {
    combination <- paste(year, reporter_iso, partner_iso, sep = ", ")
    
    if (commodity_code != "all") {
      combination <- paste(combination, commodity_code, sep = ", ")
    }
    
    message(sprintf("Downloading data for the combination %s...", combination))

    data <- try(
      fromJSON(resp$parse(encoding = "UTF-8"))
    )

    if (!is.data.frame(data)) {
      stop("It wasn't possible to obtain data. Provided this function tests your internet connection\nyou misspelled a reporter, partner or table, or there was a server problem. Please check and try again.")
    }

    return(data)
  } else if (max_attempts == 0) {
    # when attempts run out, stop with an error
    stop("Cannot connect to the API. Either the server is down or there is a connection problem.")
  } else {
    # otherwise, sleep a second and try again
    Sys.sleep(1)
    ots_read_from_api(year, reporter_iso, partner_iso, commodity_code, table, 
                      max_attempts = max_attempts - 1,
                      use_localhost
    )
  }
}
