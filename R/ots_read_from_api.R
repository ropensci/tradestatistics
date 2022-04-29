#' Reads data from the API (internal function)
#' @description Accesses \code{api.tradestatistics.io} and
#' performs different API calls to return \code{data.frames} by reading 
#' \code{JSON} data. The parameters here are passed from 
#' \code{ots_create_tidy_data}.
#' @importFrom crul HttpClient
#' @keywords internal
ots_read_from_api <- function(year = NULL,
                              reporter_iso = NULL,
                              partner_iso = NULL,
                              commodity_code = "all",
                              section_code = "all",
                              table = "yr",
                              max_attempts = 5) {
  stopifnot(max_attempts > 0)

  if (any(table %in% c("countries", "commodities", "commodities_short", "sections", "sections_colors", "tables"))) {
    message("The requested table is included within the package.")
    return(TRUE)
  }
  
  url <- switch(
    table,
    "reporters" = sprintf("reporters?y=%s", year),
    "partners" = sprintf("partners?y=%s", year),
    "yrpc" = sprintf("yrpc?y=%s&r=%s&p=%s&c=%s", year, reporter_iso, partner_iso, commodity_code),
    "yrp" = sprintf("yrp?y=%s&r=%s&p=%s", year, reporter_iso, partner_iso),
    "yrc" = sprintf("yrc?y=%s&r=%s&c=%s", year, reporter_iso, commodity_code),
    "yr" = sprintf("yr?y=%s&r=%s", year, reporter_iso),
    "yc" = sprintf("yc?y=%s&c=%s", year, commodity_code),
    "rtas" = sprintf("rtas?y=%s", year),
    "tariffs" = sprintf("tariffs?y=%s&r=%s&c=%s", year, reporter_iso, commodity_code)
  )

  base_url <- "https://api.tradestatistics.io/"
  
  resp <- HttpClient$new(url = base_url)
  resp <- resp$get(url)

  # on a successful GET, return the response
  if (resp$status_code == 200) {
    combination <- paste(year, reporter_iso, partner_iso, sep = ", ")
    
    if (commodity_code != "all") {
      combination <- paste(combination, commodity_code, sep = ", ")
    }
    
    message(sprintf("Downloading data for the combination %s...", combination))

    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("`arrow` must be installed for reading parquet data to work")
    }
      
    data <- try(read_parquet(resp$content))

    if (!is.data.frame(data)) {
      stop("It wasn't possible to obtain data. Provided this function tests your internet connection\nyou misspelled a reporter, partner or table, or there was a server problem. Please check and try again.")
    }

    return(data)
  } else if (max_attempts == 0) {
    # when attempts run out, stop with an error
    stop("Cannot connect to the API. Either the server is down or there is a connection problem.")
  } else {
    # otherwise, sleep five seconds and try again
    Sys.sleep(5)
    ots_read_from_api(year, reporter_iso, partner_iso, commodity_code, table, 
                      max_attempts = max_attempts - 1)
  }
}
