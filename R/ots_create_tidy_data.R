#' Downloads and processes the data from the API to return a human-readable tibble
#' @description Accesses \code{api.tradestatistics.io} and
#' performs different API calls to transform and return tidy data.
#' @param years Year contained within the years specified in
#' api.tradestatistics.io/year_range (e.g. \code{c(2002,2004)}, \code{c(2002:2004)} or \code{2002}).
#' Default set to \code{2019}.
#' @param reporters ISO code for reporter country (e.g. \code{"chl"}, \code{"Chile"} or
#' \code{c("chl", "Peru")}). Default set to \code{"all"}.
#' @param partners ISO code for partner country (e.g. \code{"chl"}, \code{"Chile"} or
#' \code{c("chl", "Peru")}). Default set to \code{"all"}.
#' @param commodities HS commodity codes (e.g. \code{"0101"}, \code{"01"} or search
#' matches for \code{"apple"})
#' to filter commodities. Default set to \code{"all"}.
#' @param sections HS section codes (e.g. \code{"01"}). Default set to \code{"all"}.
#' @param table Character string to select the table to obtain the data.
#' Default set to \code{yr} (Year - Reporter).
#' Run \code{ots_tables} in case of doubt.
#' @param max_attempts How many times to try to download data in case the
#' API or the internet connection fails when obtaining data. Default set
#' to \code{5}.
#' @param use_cache Logical to save and load from cache. If \code{TRUE}, the results will be cached in memory
#' if \code{file} is \code{NULL} or on disk if `file` is not \code{NULL}. Default set to \code{FALSE}.
#' @param file Optional character with the full file path to save the data. Default set to \code{NULL}.
#' @param use_localhost Logical to determine if the base URL shall be localhost instead
#' of api.tradestatistics.io. Default set to \code{FALSE}.
#' @return A tibble that describes bilateral trade metrics (imports,
#' exports, trade balance and relevant metrics
#' such as exports growth w/r to last year) between a \code{reporter}
#' and \code{partner} country.
#' @importFrom data.table `:=` rbindlist setnames
#' @importFrom jsonlite fromJSON
#' @importFrom crul HttpClient
#' @export
#' @examples
#' \dontrun{
#' # The next examples can take more than 5 seconds to compute,
#' # so these are just shown without evaluation according to CRAN rules
#'
#' # Run `ots_countries` to display the full table of countries
#' # Run `ots_commodities` to display the full table of commodities
#'
#' # What does Chile export to China? (2002)
#' ots_create_tidy_data(years = 2002, reporters = "chl", partners = "chn")
#'
#' # What can we say about Horses export in Chile and the World? (2002)
#' ots_create_tidy_data(years = 2002, commodities = "010110", table = "yc")
#' ots_create_tidy_data(years = 2002, reporters = "chl", commodities = "010110", table = "yrc")
#'
#' # What can we say about the different types of apples exported by Chile? (2002)
#' ots_create_tidy_data(years = 2002, reporters = "chl", commodities = "apple", table = "yrc")
#' }
#' @keywords functions
ots_create_tidy_data <- function(years = 2019,
                                 reporters = "all",
                                 partners = "all",
                                 commodities = "all",
                                 sections = "all",
                                 table = "yr_tc",
                                 max_attempts = 5,
                                 use_cache = FALSE,
                                 file = NULL,
                                 use_localhost = FALSE) {
  if (!is.logical(use_cache)) {
    stop("use_cache must be logical.")
  }
  
  if (!any(c(is.null(file), is.character(file)))) {
    stop("file must be NULL or character.")
  }
  
  ots_cache(
    use_cache = use_cache,
    file = file,
    years = years,
    reporters = reporters,
    partners = partners,
    commodities = commodities,
    sections = sections,
    table = table,
    max_attempts = max_attempts,
    use_localhost = use_localhost
  )
}

#' Downloads and processes the data from the API to return a human-readable tibble (unmemoised, internal)
#' @description A separation of \code{ots_create_tidy_data()} for making caching optional.
#' @keywords internal
ots_create_tidy_data_unmemoised <- function(years = 2018,
                                            reporters = "usa",
                                            partners = "all",
                                            commodities = "all",
                                            sections = "all",
                                            table = "yr",
                                            max_attempts = 5,
                                            use_localhost = FALSE) {
  # Check tables ----
  if (!table %in% tradestatistics::ots_tables$table) {
    stop("The requested table does not exist. Please check the spelling or explore the 'ots_table' table provided within this package.")
  }

  # Check years ----
  year_depending_queries <- grep("^reporters|^y|^rtas|^tariffs",
    tradestatistics::ots_tables$table,
    value = T
  )

  if (use_localhost) {
    year_range <- unlist(fromJSON("http://localhost:4949/year_range"))
  } else {
    year_range <- unlist(fromJSON("https://api.tradestatistics.io/year_range"))
  }
  
  if (all(years %in% min(year_range):max(year_range)) != TRUE &
    table %in% year_depending_queries) {
    stop("Provided that the table you requested contains a 'year' field, please verify that you are requesting data contained within the years from api.tradestatistics.io/year_range.")
  }

  # Check reporters and partners ----
  reporter_depending_queries <- grep("^yr|^ysr|^tariffs",
    tradestatistics::ots_tables$table,
    value = T
  )
  
  partner_depending_queries <- grep("^yrp|^ysrp",
    tradestatistics::ots_tables$table,
    value = T
  )

  if (!is.null(reporters)) {
    if (!all(reporters %in% tradestatistics::ots_countries$country_iso) == TRUE & table %in% reporter_depending_queries) {
      reporters_iso <- reporters[reporters %in% tradestatistics::ots_countries$country_iso]
      reporters_no_iso <- reporters[!reporters %in% tradestatistics::ots_countries$country_iso]
      
      reporters_no_iso <- sapply(
        seq_along(reporters_no_iso),
        function(x) {
          y <- tradestatistics::ots_country_code(reporters_no_iso[x])
          
          if (nrow(y) == 0) {
            stop("It was not possible to find ISO codes for any of the reporters you requested. Please check ots_countries.")
          } else {
            y <- y[, .(country_iso)]
            y <- as.vector(unlist(y))
          }
          
          if (length(y) > 1) {
            stop("There are multiple matches for the reporters you requested. Please check ots_countries.")
          } else {
            return(y)
          }
        }
      )
      
      reporters <- unique(c(reporters_iso, reporters_no_iso))
    }
  }

  if (!is.null(partners)) {
    if (!all(partners %in% tradestatistics::ots_countries$country_iso) == TRUE & table %in% partner_depending_queries) {
      partners_iso <- partners[partners %in% tradestatistics::ots_countries$country_iso]
      partners_no_iso <- partners[!partners %in% tradestatistics::ots_countries$country_iso]

      partners_no_iso <- sapply(
        seq_along(partners_no_iso),
        function(x) {
          y <- tradestatistics::ots_country_code(partners_no_iso[x])
          
          if (nrow(y) == 0) {
            stop("There are multiple matches for the partners you requested. Please check ots_countries.")
          } else {
            y <- y[, .(country_iso)]
            y <- as.vector(unlist(y))
          }
          
          if (length(y) > 1) {
            stop("There are multiple matches for the partners you requested. Please check ots_countries.")
          } else {
            return(y)
          }
        }
      )
      
      partners <- unique(c(partners_iso, partners_no_iso))
    }
  }

  # Check commodity codes ----
  commodities_depending_queries <- grep("c_tc$|c_ntc$|^tariffs",
    tradestatistics::ots_tables$table,
    value = T
  )

  if (!all(as.character(commodities) %in%
    tradestatistics::ots_commodities$commodity_code) == TRUE &
    table %in% commodities_depending_queries) {

    # commodities without match (wm)
    commodities_wm <- commodities[!commodities %in%
      tradestatistics::ots_commodities$commodity_code]

    # commodity name match (pmm)
    pnm <- lapply(
      seq_along(commodities_wm),
      function(x) { tradestatistics::ots_commodity_code(commodity = commodities_wm[x]) }
    )
    pnm <- rbindlist(pnm)

    # group name match (gnm)
    gnm <- lapply(
      seq_along(commodities_wm),
      function(x) { tradestatistics::ots_commodity_code(group = commodities_wm[x]) }
    )
    gnm <- rbindlist(gnm)

    commodities_wm <- rbind(pnm, gnm, fill = TRUE)
    commodities_wm <- unique(commodities_wm[nchar(commodity_code) == 4, .(commodity_code)])
    commodities_wm <- as.vector(unlist(commodities_wm))

    commodities <- c(commodities[commodities %in%
      tradestatistics::ots_commodities$commodity_code], commodities_wm)
    
    if(length(commodities) == 0) {
      commodities <- NA
    }
  }

  if (!all(as.character(commodities) %in%
    tradestatistics::ots_commodities$commodity_code == TRUE) &
    table %in% commodities_depending_queries) {
    stop("The requested commodities do not exist. Please check ots_commodities.")
  }
  
  # Check section codes -----------------------------------------------------
  sections <- sort(as.character(sections))
  
  if (!all(sections %in% c(tradestatistics::ots_sections$section_code, "all") == TRUE) &
      table %in% commodities_depending_queries) {
    for (i in seq_along(sections)) {
      if (sections[i] != "all") {
        sections[i] <- as.integer(substr(sections, 1, 3))
      }
      if (nchar(sections[i]) != 2 & sections[i] != "999") {
        sections[i] <- paste0("0", sections[i])
      }
    }
    
    for (i in seq_along(sections)) {
      sections[i] <- if (!sections[i] %in% tradestatistics::ots_sections$section_code) {
        NA
      } else {
        sections[i]
      }
    }
    
    sections <- sections[!is.na(sections)]
    if(length(sections) == 0) {
      sections <- NA
    }
  }
  
  if (!all(sections %in% c(tradestatistics::ots_sections$section_code, "all") == TRUE) &
    table %in% commodities_depending_queries) {
    stop("The requested sections do not exist. Please check ots_sections.")
  }
  
  # Check optional parameters ----
  if (!is.numeric(max_attempts) | max_attempts <= 0) {
    stop("max_attempts must be a positive integer.")
  }

  if (!is.logical(use_localhost)) {
    stop("use_localhost must be logical.")
  }

  # Read from API ----
  if (!table %in% commodities_depending_queries & any(commodities != "all") == TRUE) {
    commodities <- "all"
    warning("The commodities argument will be ignored provided that you requested a table without commodity_code field.")
  }
  
  if (grepl("^ysrpc", table) == TRUE &
      !all(c(reporters, partners) == "all") == TRUE) {
    reporters <- "all"
    partners <- "all"
    warning("The reporter and partner arguments will be ignored provided that you requested a table without these filtering options.")
  }
  
  if (is.null(reporters)) {
    reporters <- "all"
    warning("No reporter was specified, therefore all available reporters will be returned.")
  }

  if (is.null(partners)) {
    partners <- "all"
    warning("No partner was specified, therefore all available partners will be returned.")
  }

  condensed_parameters <- expand.grid(
    year = years,
    reporter = reporters,
    partner = partners,
    commodity = commodities,
    section = if (grepl("^ysrpc", table)) { sections } else { "all" },
    stringsAsFactors = FALSE
  )

  data <- lapply(
    seq_len(nrow(condensed_parameters)),
    function(x) {
      ots_read_from_api(
        table = table,
        max_attempts = max_attempts,
        year = condensed_parameters$year[x],
        reporter_iso = condensed_parameters$reporter[x],
        partner_iso = condensed_parameters$partner[x],
        commodity_code = condensed_parameters$commodity[x],
        section_code = condensed_parameters$section[x],
        use_localhost = use_localhost
      )
    }
  )
  data <- rbindlist(data, fill = TRUE)
  
  # no data in API message
  if (any("observation" %in% names(data))) {
    warning("The parameters you specified resulted in API calls returning 0 rows.")
    return(data)
  }

  # Add attributes based on codes, etc (and join years, if applicable) ------

  # include countries data
  if (table %in% reporter_depending_queries) {
    data <- merge(data, tradestatistics::ots_countries[, .(country_iso, country_name_english)],
                  all.x = TRUE, all.y = FALSE,
                  by.x = "reporter_iso", by.y = "country_iso",
                  allow.cartesian = TRUE)
    data <- setnames(data, "country_name_english", "reporter_name")
  }

  if (table %in% partner_depending_queries) {
    data <- merge(data, tradestatistics::ots_countries[, .(country_iso, country_name_english)],
                  all.x = TRUE, all.y = FALSE,
                  by.x = "partner_iso", by.y = "country_iso",
                  allow.cartesian = TRUE)
    data <- setnames(data, "country_name_english", "partner_name")
  }
  
  # include commodities data
  if (table %in% commodities_depending_queries) {
    if (!grepl("ysrpc", table)) {
      data <- merge(data, tradestatistics::ots_commodities,
                    all.x = TRUE, all.y = FALSE,
                    by.x = "commodity_code", by.y = "commodity_code",
                    allow.cartesian = TRUE)
    } else {
      data <- merge(data, tradestatistics::ots_commodities,
                    all.x = TRUE, all.y = FALSE,
                    by.x = c("commodity_code", "section_code"), by.y = c("commodity_code", "section_code"),
                    allow.cartesian = TRUE)
    }
    
    data <- setnames(data, c("commodity_fullname_english", "section_fullname_english"), 
                     c("commodity_name", "section_name"))
  }
  
  columns_order <- c("year",
                     grep("^reporter_", colnames(data), value = TRUE),
                     grep("^partner_", colnames(data), value = TRUE),
                     grep("^commodity_", colnames(data), value = TRUE),
                     grep("^section_", colnames(data), value = TRUE),
                     grep("^trade_", colnames(data), value = TRUE),
                     grep("^country|^rta", colnames(data), value = TRUE),
                     grep("rate|average|line", colnames(data), value = TRUE)
  )

  data <- data[, ..columns_order]

  return(data)
}

#' Downloads and processes the data from the API to return a human-readable tibble (memoised, internal)
#' @description A composition of \code{ots_create_tidy_data_unmemoised()} and \code{memoise()} for caching the output
#' @importFrom memoise memoise
#' @keywords internal
ots_create_tidy_data_memoised <- memoise::memoise(ots_create_tidy_data_unmemoised)
