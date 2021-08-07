#' Downloads and processes the data from the API to return a human-readable tibble
#' @description Accesses \code{api.tradestatistics.io} and
#' performs different API calls to transform and return tidy data.
#' @param years Year contained within the years specified in
#' api.tradestatistics.io/year_range (e.g. \code{c(1980,1985)}, \code{c(1980:1981)} or \code{1980}).
#' Default set to \code{1962}.
#' @param reporters ISO code for reporter country (e.g. \code{"chl"}, \code{"Chile"} or
#' \code{c("chl", "Peru")}). Default set to \code{"all"}.
#' @param partners ISO code for partner country (e.g. \code{"chl"}, \code{"Chile"} or
#' \code{c("chl", "Peru")}). Default set to \code{"all"}.
#' @param commodities HS commodity codes (e.g. \code{"0101"}, \code{"01"} or search
#' matches for \code{"apple"})
#' to filter commodities. Default set to \code{"all"}.
#' @param groups HS commodity groups (e.g. \code{"01"} or search matches for 
#' \code{"animals"} with \code{ots_commodity_code}) to filter groups. Default set to \code{"all"}.
#' @param communities unofficial commodity communities (e.g. \code{"01"} or see 
#' the table \code{ots_communities}). Default set to \code{"all"}.
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
#' # What does Chile export to China? (1980)
#' ots_create_tidy_data(years = 1980, reporters = "chl", partners = "chn")
#'
#' # What can we say about Horses export in Chile and the World? (1980)
#' ots_create_tidy_data(years = 1980, commodities = "0101", table = "yc")
#' ots_create_tidy_data(years = 1980, reporters = "chl", commodities = "0101", table = "yrc")
#'
#' # What can we say about the different types of apples exported by Chile? (1980)
#' ots_create_tidy_data(years = 1980, reporters = "chl", commodities = "apple", table = "yrc")
#' }
#' @keywords functions
ots_create_tidy_data <- function(years = 2018,
                                 reporters = "all",
                                 partners = "all",
                                 commodities = "all",
                                 groups = "all",
                                 communities = "all",
                                 table = "yr",
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
    groups = groups,
    communities = communities,
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
                                            groups = "all",
                                            communities = "all",
                                            table = "yr",
                                            max_attempts = 5,
                                            use_localhost = FALSE) {
  # Check tables ----
  if (!table %in% ots_tables$table) {
    stop("The requested table does not exist. Please check the spelling or explore the 'ots_table' table provided within this package.")
  }

  # Check years ----
  year_depending_queries <- grep("^reporters|^y",
    ots_tables$table,
    value = T
  )

  # update this when new data is added ----
  max_year <- fromJSON("https://api.tradestatistics.io/years")[1,2]
  year_range <- c(1962, max_year)
  
  if (all(years %in% min(year_range):max(year_range)) != TRUE &
    table %in% year_depending_queries) {
    stop("Provided that the table you requested contains a 'year' field, please verify that you are requesting data contained within the years from api.tradestatistics.io/year_range.")
  }

  # Check reporters and partners ----
  reporter_depending_queries <- grep("^yr",
    ots_tables$table,
    value = T
  )
  
  partner_depending_queries <- grep("^yrp",
    ots_tables$table,
    value = T
  )

  if (!is.null(reporters)) {
    if (!all(reporters %in% ots_countries$country_iso) == TRUE & table %in% reporter_depending_queries) {
      reporters_iso <- reporters[reporters %in% ots_countries$country_iso]
      reporters_no_iso <- reporters[!reporters %in% ots_countries$country_iso]
      
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
    if (!all(partners %in% ots_countries$country_iso) == TRUE & table %in% partner_depending_queries) {
      partners_iso <- partners[partners %in% ots_countries$country_iso]
      partners_no_iso <- partners[!partners %in% ots_countries$country_iso]

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
  commodities_depending_queries <- grep("c$",
    ots_tables$table,
    value = T
  )

  if (!all(as.character(commodities) %in%
    ots_commodities$commodity_code) == TRUE &
    table %in% commodities_depending_queries) {

    # commodities without match (wm)
    commodities_wm <- commodities[!commodities %in%
      ots_commodities$commodity_code]

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
      ots_commodities$commodity_code], commodities_wm)
    
    if(length(commodities) == 0) {
      commodities <- NA
    }
  }

  if (!all(as.character(commodities) %in%
    ots_commodities$commodity_code == TRUE) &
    table %in% commodities_depending_queries) {
    stop("The requested commodities do not exist. Please check ots_commodities.")
  }

  # Check groups ----
  groups_depending_queries <- grep("-groups$",
                                    ots_tables$table,
                                    value = T
  )
  
  unique_groups <- unique(ots_commodities$group_code)
  unique_groups <- c(unique_groups[!is.na(unique_groups)], "all")
  
  if (!all(as.character(groups) %in% unique_groups) == TRUE &
      table %in% groups_depending_queries) {
    
    # groups without match (wm)
    groups_wm <- groups[!groups %in% unique_groups]
    
    # group name match (gmm)
    gnm <- lapply(
      seq_along(groups_wm),
      function(x) { tradestatistics::ots_commodity_code(group = groups_wm[x]) }
    )
    gnm <- rbindlist(gnm)
    
    groups_wm <- unique(gnm[, .(group_code)])
    groups_wm <- as.vector(unlist(groups_wm))
    
    groups <- c(groups[groups %in% unique_groups], groups_wm)
    
    if(length(groups) == 0) {
      groups <- NA
    }
  }
  
  if (!all(as.character(groups) %in% unique_groups == TRUE) &
      table %in% groups_depending_queries) {
    stop("The requested groups do not exist. Please check ots_commodities.")
  }
  
  # Check communities ----
  communities_depending_queries <- grep("-communities$",
                                      ots_tables$table,
                                      value = T
  )
  
  unique_communities <- unique(tradestatistics::ots_communities$community_code)
  unique_communities <- c(unique_communities, "all")
  
  if (!all(as.character(communities) %in% unique_communities) == TRUE &
      table %in% communities_depending_queries) {
    
    # communities without match (wm)
    communities_wm <- communities[!communities %in% unique_communities]
    
    # communities name match (snm)
    cnm <- lapply(
      seq_along(communities_wm),
      function(x) { tradestatistics::ots_commodity_community(community = communities_wm[x]) }
    )
    cnm <- rbindlist(cnm)
    
    communities_wm <- unique(snm[, .(community_code)])
    communities_wm <- as.vector(unlist(communities_wm))
    
    communities <- c(communities[communities %in% unique_communities], communities_wm)
    
    if(length(communities) == 0) {
      communities <- NA
    }
  }
  
  if (!all(as.character(communities) %in% unique_communities == TRUE) &
      table %in% communities_depending_queries) {
    stop("The requested communities do not exist. Please check ots_commodities.")
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
  
  if (!table %in% groups_depending_queries & any(groups != "all") == TRUE) {
    groups <- "all"
    warning("The groups argument will be ignored provided that you requested a table without group_code field.")
  }
  
  if (!table %in% communities_depending_queries & any(communities != "all") == TRUE) {
    communities <- "all"
    warning("The communities argument will be ignored provided that you requested a table without community_code field.")
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
    group = groups,
    community = communities,
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
        group_code = condensed_parameters$group[x],
        community_code = condensed_parameters$community[x],
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
    if (table %in% partner_depending_queries) {
      data <- merge(data, tradestatistics::ots_countries[, .(country_iso, country_fullname_english)],
                    all.x = TRUE, all.y = FALSE,
                    by.x = "reporter_iso", by.y = "country_iso",
                    allow.cartesian = TRUE)
      data <- setnames(data, "country_fullname_english", "reporter_fullname_english")
    } else {
      data <- merge(data, tradestatistics::ots_countries[, .(country_iso, country_fullname_english)],
                    all.x = TRUE, all.y = FALSE,
                    by.x = "reporter_iso", by.y = "country_iso",
                    allow.cartesian = TRUE)
      data <- setnames(data, "country_fullname_english", "reporter_fullname_english")
    }
  }

  if (table %in% partner_depending_queries) {
    data <- merge(data, tradestatistics::ots_countries[, .(country_iso, country_fullname_english)],
                  all.x = TRUE, all.y = FALSE,
                  by.x = "partner_iso", by.y = "country_iso",
                  allow.cartesian = TRUE)
    data <- setnames(data, "country_fullname_english", "partner_fullname_english")
  }
  
  # include commodities data
  if (table %in% commodities_depending_queries) {
    data <- merge(data, tradestatistics::ots_commodities,
                  all.x = TRUE, all.y = FALSE,
                  by.x = "commodity_code", by.y = "commodity_code",
                  allow.cartesian = TRUE)
    
    data <- merge(data, tradestatistics::ots_commodities_shortnames,
                  all.x = TRUE, all.y = FALSE,
                  by.x = "commodity_code", by.y = "commodity_code",
                  allow.cartesian = TRUE)
    
    data <- merge(data, tradestatistics::ots_communities,
                  all.x = TRUE, all.y = FALSE,
                  by.x = "commodity_code", by.y = "commodity_code",
                  allow.cartesian = TRUE)
  }

  # include groups data
  if (table %in% groups_depending_queries) {
    ots_groups <- unique(ots_commodities[, c("group_code", "group_fullname_english")])
    ots_groups <- ots_groups[!is.na(ots_groups$group_code), ]
    
    if (any(colnames(data) %in% "top_export_group_code")) {
      data <- merge(data, ots_groups,
                    all.x = TRUE, all.y = FALSE,
                    by.x = "group_code", by.y = "group_code",
                    allow.cartesian = TRUE)
    }
    
    if (any(colnames(data) %in% "top_export_group_code")) {
      data <- merge(data, ots_groups,
                    all.x = TRUE, all.y = FALSE,
                    by.x = "top_export_group_code", by.y = "group_code",
                    allow.cartesian = TRUE)
      data <- setnames(data, "group_fullname_english", "top_export_group_fullname_english")
      
      data <- merge(data, ots_groups,
                    all.x = TRUE, all.y = FALSE,
                    by.x = "top_import_group_code", by.y = "group_code",
                    allow.cartesian = TRUE)
      data <- setnames(data, "group_fullname_english", "top_import_group_fullname_english")
    }
  }
  
  if (table == "yr") {
    data <- merge(data, tradestatistics::ots_commodities,
                  all.x = TRUE, all.y = FALSE,
                  by.x = "commodity_code_top_exp", by.y = "commodity_code",
                  allow.cartesian = TRUE)
    data <- setnames(data, c("commodity_fullname_english", "group_code", "group_fullname_english"),
                     c("commodity_fullname_english_top_exp", "group_code_top_exp", "group_fullname_english_top_exp"))
    
    data <- merge(data, tradestatistics::ots_commodities,
                  all.x = TRUE, all.y = FALSE,
                  by.x = "commodity_code_top_imp", by.y = "commodity_code",
                  allow.cartesian = TRUE)
    data <- setnames(data, c("commodity_fullname_english", "group_code", "group_fullname_english"),
                     c("commodity_fullname_english_top_imp", "group_code_top_imp", "group_fullname_english_top_imp"))
    
    data <- merge(data, tradestatistics::ots_communities,
                  all.x = TRUE, all.y = FALSE,
                  by.x = "commodity_code_top_exp", by.y = "commodity_code",
                  allow.cartesian = TRUE)
    data <- setnames(data, c("community_code", "community_name", "community_color"),
                     c("community_code_top_exp", "community_name_top_exp", "community_color_top_exp"))
    
    data <- merge(data, tradestatistics::ots_communities,
                  all.x = TRUE, all.y = FALSE,
                  by.x = "commodity_code_top_imp", by.y = "commodity_code",
                  allow.cartesian = TRUE)
    data <- setnames(data, c("community_code", "community_name", "community_color"),
                     c("community_code_top_imp", "community_name_top_imp", "community_color_top_imp"))
  }
  
  columns_order <- c("year",
                     grep("^reporter_", colnames(data), value = TRUE),
                     grep("^partner_", colnames(data), value = TRUE),
                     grep("^commodity_", colnames(data), value = TRUE),
                     grep("^group_", colnames(data), value = TRUE),
                     grep("^community_", colnames(data), value = TRUE),
                     grep("^trade_", colnames(data), value = TRUE)
  )

  data <- data[, ..columns_order]

  return(data)
}

#' Downloads and processes the data from the API to return a human-readable tibble (memoised, internal)
#' @description A composition of \code{ots_create_tidy_data_unmemoised()} and \code{memoise()} for caching the output
#' @importFrom memoise memoise
#' @keywords internal
ots_create_tidy_data_memoised <- memoise::memoise(ots_create_tidy_data_unmemoised)
