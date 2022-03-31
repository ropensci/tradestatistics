#' Reads data from the API (internal function)
#' @description Accesses \code{api.tradestatistics.io} and
#' performs different API calls to return \code{data.frames} by reading 
#' \code{JSON} data. The parameters here are passed from 
#' \code{ots_create_tidy_data}.
#' @importFrom jsonlite fromJSON
#' @importFrom crul HttpClient
#' @keywords internal
ots_read_from_api <- function(year = NULL,
                              reporter_iso = NULL,
                              partner_iso = NULL,
                              commodity_code = "all",
                              section_code = "all",
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
    "yrpc_tc" = sprintf("yrpc_tc?y=%s&r=%s&p=%s&c=%s", year, reporter_iso, partner_iso, commodity_code),
    "yrpc_ntc" = sprintf("yrpc_ntc?y=%s&r=%s&p=%s&c=%s", year, reporter_iso, partner_iso, commodity_code),
    "ysrpc_tc" = sprintf("ysrpc_tc?y=%s&s=%s", year, section_code),
    "ysrpc_ntc" = sprintf("ysrpc_ntc?y=%s&s=%s", year, section_code),
    "yrp_tc" = sprintf("yrp_tc?y=%s&r=%s&p=%s", year, reporter_iso, partner_iso),
    "yrp_ntc" = sprintf("yrp_ntc?y=%s&r=%s&p=%s", year, reporter_iso, partner_iso),
    "yrc_tc" = sprintf("yrc_tc?y=%s&r=%s&c=%s", year, reporter_iso, commodity_code),
    "yrc_ntc" = sprintf("yrc_ntc?y=%s&r=%s&c=%s", year, reporter_iso, commodity_code),
    "yr_tc" = sprintf("yr_tc?y=%s&r=%s", year, reporter_iso),
    "yr_ntc" = sprintf("yr_ntc?y=%s&r=%s", year, reporter_iso),
    "years" = "years",
    "rtas" = sprintf("rtas?y=%s", year),
    "tariffs" = sprintf("tariffs?y=%s&r=%s&c=%s", year, reporter_iso, commodity_code)
  )

  if (use_localhost == TRUE) {
    base_url <- "http://localhost:4949/"
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

    if (!grepl("^yrpc|^ysrpc", url)) {
      data <- try(fromJSON(resp$parse(encoding = "UTF-8")))
    } else {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop("`arrow` must be installed for reading parquet data to work")
      }
      
      data <- try(arrow::read_parquet(resp$content))
    }
    
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
