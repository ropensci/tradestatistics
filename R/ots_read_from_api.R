#' Reads data from the API and returns a dataframe
#' @description This function accesses \code{api.tradestatistics.io} and
#' performs different API calls to return \code{data.frames} by reading \code{JSON} data
#' @param years Numeric value greater or equal to 1962 and lower of equal
#' to 2016. Default set to \code{NULL}.
#' @param reporter ISO code for country of reporter (e.g. \code{chl} for
#' Chile). Default set to \code{NULL}.
#' @param partner ISO code for country of partner (e.g. \code{chn} for
#' China). Default set to \code{NULL}.
#' @param commodity_code_length Character string to indicate the granularity level on commodities.
#' Default set to \code{4}.
#' @param table Character string to select the table to obtain the data. Default set to \code{yrpc}
#' (Year - Reporter - Partner - Commodity).
#' @param max_attempts Number of attempts to retry in case of data retrieving failure.
#' Default set to \code{5}.
#' \code{ots_create_tidy_data}.
#' @importFrom jsonlite fromJSON
#' @importFrom crul HttpClient
#' @export
#' @examples
#' \dontrun{
#' # What does Chile export to China? (2015)
#' ots_read_from_api(years = 2015, reporter = "chl", partner = "chn")
#' }
#' @keywords functions

ots_read_from_api <- function(years = NULL,
                              reporter = NULL,
                              partner = NULL,
                              commodity_code_length = 4,
                              table = "yrpc",
                              max_attempts = 5) {
  stopifnot(max_attempts > 0)

  url <- switch(
    table,
    "countries" = "countries",
    "products" = "products",
    "reporters" = sprintf("reporters?y=%s", years),
    "country_rankings" = sprintf("country_rankings?y=%s", years),
    "product_rankings" = sprintf("product_rankings?y=%s", years),
    "yrpc" = sprintf(
      "yrpc?y=%s&r=%s&p=%s&l=%s",
      years, reporter, partner, commodity_code_length
    ),
    "yrp" = sprintf("yrp?y=%s&r=%s&p=%s", years, reporter, partner),
    "yrc" = sprintf(
      "yrc?y=%s&r=%s&l=%s",
      years, reporter, commodity_code_length
    ),
    "yr" = sprintf("yr?y=%s&r=%s", years, reporter),
    "yc" = sprintf("yc?y=%s&l=%s", years, commodity_code_length)
  )

  resp <- crul::HttpClient$new(url = "https://api.tradestatistics.io/")
  resp <- resp$get(url)

  # on a successful GET, return the response
  if (resp$status_code == 200) {
    sprintf("Trying to download data for the year %s...", years)

    data <- try(
      jsonlite::fromJSON(resp$parse(encoding = "UTF-8"))
    )

    if (!is.data.frame(data)) {
      stop(
        "
        It wasn't possible to obtain the requested data. Please check our Twitter to
        see if we are reporting server problems right now and try again later.
        Another possibility is that you are behind a firewall that restricts how
        R connects to the Internet.
        "
      )
    }

    message(sprintf("The data for the year %s was downloaded without problems.", years))

    return(data)
  } else if (max_attempts == 0) {
    # when attempts run out, stop with an error
    stop(
      "
      R couldn't sucessfully connect to the API. Please check our Twitter to
      see if we are reporting server problems right now and try again later.
      Another possibility is that you are behind a firewall that restricts how
      R connects to the Internet.
      "
    )
  } else {
    # otherwise, sleep a second and try again
    Sys.sleep(1)
    ots_read_from_api(
      years, reporter, partner, commodity_code_length, table,
      max_attempts = max_attempts - 1
    )
  }
}
