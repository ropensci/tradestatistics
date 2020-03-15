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
#' @param products HS product codes (e.g. \code{"0101"}, \code{"01"} or search
#' matches for \code{"apple"})
#' to filter products. Default set to \code{"all"}.
#' @param groups HS product groups (e.g. \code{"01"} or search matches for 
#' \code{"animals"}) to filter groups. Default set to \code{"all"}.
#' @param communities unofficial product communities (e.g. \code{"01"} or search
#' matches for \code{"animals"}) to filter communities Default set to
#' \code{"all"}.
#' @param table Character string to select the table to obtain the data.
#' Default set to \code{yrpc} (Year - Reporter - Partner - Product Code).
#' Run \code{ots_tables} in case of doubt.
#' @param max_attempts How many times to try to download data in case the
#' API or the internet connection fails when obtaining data. Default set
#' to \code{5}.
#' @param include_shortnames Whether to include or not to include unofficial shortened product names.
#' Default set to \code{FALSE}.
#' @param include_communities Whether to include or not to include product communities.
#' Default set to \code{FALSE}.
#' @param use_localhost Logical to determine if the base URL shall be localhost instead
#' of api.tradestatistics.io. Default set to \code{FALSE}.
#' @param use_cache Logical to save and load from cache. If \code{TRUE}, the results will be cached in memory
#' if \code{file} is \code{NULL} or on disk if `file` is not \code{NULL}. Default set to \code{FALSE}.
#' @param file Optional character with the full file path to save the data. Default set to \code{NULL}.
#' @return A tibble that describes bilateral trade metrics (imports,
#' exports, trade balance and relevant metrics
#' such as exports growth w/r to last year) between a \code{reporter}
#' and \code{partner} country.
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr as_tibble select filter everything
#' left_join bind_rows rename distinct starts_with
#' @importFrom rlang sym syms
#' @importFrom purrr map_chr map_df as_vector
#' @importFrom jsonlite fromJSON
#' @importFrom crul HttpClient
#' @export
#' @examples
#' \dontrun{
#' # The next examples can take more than 5 seconds to compute,
#' # so these are just shown without evaluation according to CRAN rules
#'
#' # Run `ots_countries` to display the full table of countries
#' # Run `ots_products` to display the full table of products
#'
#' # What does Chile export to China? (1980)
#' ots_create_tidy_data(years = 1980, reporters = "chl", partners = "chn")
#'
#' # What can we say about Horses export in Chile and the World? (1980)
#' ots_create_tidy_data(years = 1980, products = "0101", table = "yc")
#' ots_create_tidy_data(years = 1980, reporters = "chl", products = "0101", table = "yrc")
#'
#' # What can we say about the different types of apples exported by Chile? (1980)
#' ots_create_tidy_data(years = 1980, reporters = "chl", products = "apple", table = "yrc")
#' }
#' @keywords functions
ots_create_tidy_data <- function(years = 2018,
                                 reporters = "usa",
                                 partners = "all",
                                 products = "all",
                                 groups = "all",
                                 communities = "all",
                                 table = "yrpg",
                                 max_attempts = 5,
                                 include_shortnames = FALSE,
                                 include_communities = FALSE,
                                 use_localhost = FALSE,
                                 use_cache = FALSE,
                                 file = NULL) {
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
    products = products,
    groups = groups,
    communities = communities,
    table = table,
    max_attempts = max_attempts,
    include_shortnames = include_shortnames,
    include_communities = include_communities,
    use_localhost = use_localhost
  )
}

#' Downloads and processes the data from the API to return a human-readable tibble (unmemoised, internal)
#' @description A separation of \code{ots_create_tidy_data()} for making caching optional.
#' @keywords internal
ots_create_tidy_data_unmemoised <- function(years = 2018,
                                            reporters = "usa",
                                            partners = "all",
                                            products = "all",
                                            groups = "all",
                                            communities = "all",
                                            table = "yrpg",
                                            max_attempts = 5,
                                            include_shortnames = FALSE,
                                            include_communities = FALSE,
                                            use_localhost = FALSE) {
  # Check tables ----
  if (!table %in% tradestatistics::ots_tables$table) {
    stop("The requested table does not exist. Please check the spelling or explore the 'ots_table' table provided within this package.")
  }

  # Check years ----
  year_depending_queries <- grep("^reporters|rankings$|^y",
    tradestatistics::ots_tables$table,
    value = T
  )

  url <- "year_range"

  resp <- HttpClient$new(url = "https://api.tradestatistics.io/")
  resp <- resp$get(url)

  year_range <- as_vector(fromJSON(resp$parse(encoding = "UTF-8")))

  if (all(years %in% min(year_range):max(year_range)) != TRUE &
    table %in% year_depending_queries) {
    stop("Provided that the table you requested contains a 'year' field, please verify that you are requesting data contained within the years from api.tradestatistics.io/year_range.")
  }

  # Check reporters and partners ----
  reporter_depending_queries <- grep("^yr",
    tradestatistics::ots_tables$table,
    value = T
  )
  
  partner_depending_queries <- grep("^yrp",
    tradestatistics::ots_tables$table,
    value = T
  )

  if (!is.null(reporters)) {
    if (!all(reporters %in% tradestatistics::ots_countries$country_iso) == TRUE & table %in% reporter_depending_queries) {
      reporters_iso <- reporters[reporters %in% tradestatistics::ots_countries$country_iso]
      reporters_no_iso <- reporters[!reporters %in% tradestatistics::ots_countries$country_iso]
      
      reporters_no_iso <- map_chr(seq_along(reporters_no_iso),
              function(x) {
                y <- tradestatistics::ots_country_code(reporters_no_iso[x]) %>% 
                  select(!!sym("country_iso")) %>% 
                  as_vector()
                
                if (length(y) > 1) {
                  stop("There are multiple matches for the reporters you requested. Please check ots_countries.")
                } else {
                  return(y)
                }
              }
      )
      
      reporters <- unique(c(reporters_iso, reporters_no_iso))
      
      if (length(reporters) == 0) {
        stop("It was not possible to find ISO codes for any of the reporters you requested. Please check ots_countries.")
      }
    }
  }

  if (!is.null(partners)) {
    if (!all(partners %in% tradestatistics::ots_countries$country_iso) == TRUE & table %in% partner_depending_queries) {
      partners_iso <- partners[partners %in% tradestatistics::ots_countries$country_iso]
      partners_no_iso <- partners[!partners %in% tradestatistics::ots_countries$country_iso]

      partners_no_iso <- map_chr(seq_along(partners_no_iso),
                                  function(x) {
                                    y <- tradestatistics::ots_country_code(partners_no_iso[x]) %>% 
                                      select(!!sym("country_iso")) %>% 
                                      as_vector()
                                    
                                    if (length(y) > 1) {
                                      stop("There are multiple matches for the reporters you requested. Please check ots_countries.")
                                    } else {
                                      return(y)
                                    }
                                  }
      )
      
      partners <- unique(c(partners_iso, partners_no_iso))
      
      if (length(partners) == 0) {
        stop("It was not possible to find ISO codes for any of the partners you requested. Please check ots_countries.")
      }
    }
  }

  # Check product codes ----
  product_depending_queries <- grep("c$",
    tradestatistics::ots_tables$table,
    value = T
  )

  if (!all(as.character(products) %in%
    tradestatistics::ots_products$product_code) == TRUE &
    table %in% product_depending_queries) {

    # products without match (wm)
    products_wm <- products[!products %in%
      tradestatistics::ots_products$product_code]

    # product name match (pmm)
    pnm <- map_df(
      .x = seq_along(products_wm),
      ~ tradestatistics::ots_product_code(productname = products_wm[.x])
    )

    # group name match (gnm)
    gnm <- map_df(
      .x = seq_along(products_wm),
      ~ tradestatistics::ots_product_code(productgroup = products_wm[.x])
    )

    products_wm <- bind_rows(pnm, gnm) %>%
      distinct(!!sym("product_code")) %>%
      as_vector()

    products <- c(products[products %in%
      tradestatistics::ots_products$product_code], products_wm)
    
    if(length(products) == 0) {
      products <- NA
    }
  }

  if (!all(as.character(products) %in%
    tradestatistics::ots_products$product_code == TRUE) &
    table %in% product_depending_queries) {
    stop("The requested products do not exist. Please check ots_products.")
  }

  # Check groups ----
  group_depending_queries <- grep("g$",
                                    tradestatistics::ots_tables$table,
                                    value = T
  )
  
  unique_groups <- unique(tradestatistics::ots_products$group_code)
  unique_groups <- c(unique_groups[!is.na(unique_groups)], "all")
  
  if (!all(as.character(groups) %in% unique_groups) == TRUE &
      table %in% group_depending_queries) {
    
    # groups without match (wm)
    groups_wm <- groups[!groups %in% unique_groups]
    
    # group name match (gmm)
    gnm <- map_df(
      .x = seq_along(groups_wm),
      ~ tradestatistics::ots_product_code(productgroup = groups_wm[.x])
    )
    
    groups_wm <- gnm %>%
      distinct(!!sym("group_code")) %>%
      as_vector()
    
    groups <- c(groups[groups %in% unique_groups], groups_wm)
    
    if(length(groups) == 0) {
      groups <- NA
    }
  }
  
  if (!all(as.character(groups) %in% unique_groups == TRUE) &
      table %in% group_depending_queries) {
    stop("The requested groups do not exist. Please check ots_products.")
  }
  
  # Check communities ----
  community_depending_queries <- grep("o$",
                                      tradestatistics::ots_tables$table,
                                      value = T
  )
  
  unique_communities <- unique(tradestatistics::ots_communities$community_code)
  unique_communities <- c(unique_communities, "all")
  
  if (!all(as.character(communities) %in% unique_communities) == TRUE &
      table %in% group_depending_queries) {
    
    # communities without match (wm)
    communities_wm <- communities[!communities %in% unique_communities]
    
    # community name match (cmm)
    cnm <- map_df(
      .x = seq_along(communities_wm),
      ~ tradestatistics::ots_product_community(productcommunity = communities_wm[.x])
    )
    
    communities_wm <- cnm %>%
      distinct(!!sym("community_code")) %>%
      as_vector()
    
    communities <- c(communities[communities %in% unique_communities], communities_wm)
    
    if(length(communities) == 0) {
      communities <- NA
    }
  }
  
  if (!all(as.character(communities) %in% unique_communities == TRUE) &
      table %in% community_depending_queries) {
    stop("The requested communities do not exist. Please check ots_products.")
  }
  
  # Check optional parameters ----
  if (!is.numeric(max_attempts) | max_attempts <= 0) {
    stop("max_attempts must be a positive integer.")
  }

  if (!is.logical(include_shortnames)) {
    stop("include_shortnames must be logical.")
  }

  if (!is.logical(include_communities)) {
    stop("include_communities must be logical.")
  }

  if (!is.logical(use_localhost)) {
    stop("use_localhost must be logical.")
  }

  # Read from API ----
  if (!table %in% product_depending_queries & any(products != "all") == TRUE) {
    products <- "all"
    warning("The products argument will be ignored provided that you requested a table without product_code field.")
  }
  
  if (!table %in% group_depending_queries & any(groups != "all") == TRUE) {
    groups <- "all"
    warning("The groups argument will be ignored provided that you requested a table without group_code field.")
  }
  
  if (!table %in% community_depending_queries & any(communities != "all") == TRUE) {
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
    product = products,
    group = groups,
    community = communities,
    stringsAsFactors = FALSE
  )

  data <- map_df(
    .x = seq_len(nrow(condensed_parameters)),
    ~ ots_read_from_api(
      table = table,
      max_attempts = max_attempts,
      year = condensed_parameters$year[.x],
      reporter = condensed_parameters$reporter[.x],
      partner = condensed_parameters$partner[.x],
      product_code = condensed_parameters$product[.x],
      group_code = condensed_parameters$group[.x],
      community_code = condensed_parameters$community[.x],
      use_localhost = use_localhost
    )
  ) %>%
    as_tibble()

  # no data in API message
  if (any("observation" %in% names(data))) {
    warning("Rows with NAs in both exports and imports will be dropped.")
    
    data <- data %>% 
      filter(is.na(!!sym("observation"))) %>% 
      select(-!!sym("observation"))
  }

  # Add attributes based on codes, etc (and join years, if applicable) ------

  # include countries data
  if (table %in% reporter_depending_queries) {
    if (table %in% partner_depending_queries) {
      data %<>%
        left_join(select(
          tradestatistics::ots_countries,
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
      data %<>%
        left_join(select(
          tradestatistics::ots_countries,
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

  if (table %in% partner_depending_queries) {
    data %<>%
      left_join(select(
        tradestatistics::ots_countries,
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
  if (table %in% product_depending_queries) {
    data %<>%
      left_join(tradestatistics::ots_products, by = "product_code")

    if (table == "yrpc") {
      data %<>%
        select(
          !!!syms(c(
            "year",
            "reporter_iso",
            "partner_iso",
            "reporter_fullname_english",
            "partner_fullname_english",
            "product_code",
            "product_fullname_english",
            "group_code",
            "group_name"
          )),
          everything()
        )
    }

    if (table == "yrc") {
      data %<>%
        select(
          !!!syms(c(
            "year",
            "reporter_iso",
            "reporter_fullname_english",
            "product_code",
            "product_fullname_english",
            "group_code",
            "group_name"
          )),
          everything()
        )
    }

    if (table == "yc") {
      data %<>%
        select(
          !!!syms(c(
            "year",
            "product_code",
            "product_fullname_english",
            "group_code",
            "group_name"
          )),
          everything()
        )
    }
  }

  if (table %in% product_depending_queries & include_shortnames == TRUE) {
    data %<>%
      left_join(tradestatistics::ots_product_shortnames)
  }

  if (table %in% product_depending_queries & include_communities == TRUE) {
    data %<>%
      left_join(tradestatistics::ots_communities)
  }

  # include groups data
  if (table %in% group_depending_queries) {
    data %<>%
      left_join(
        tradestatistics::ots_products %>% 
          select(!!!syms(c("group_code", "group_name"))) %>% 
          distinct(), by = "group_code"
      )
    
    if (table == "yrpg") {
      data %<>%
        select(
          !!!syms(c(
            "year",
            "reporter_iso",
            "partner_iso",
            "reporter_fullname_english",
            "partner_fullname_english",
            "group_code",
            "group_name"
          )),
          everything()
        )
    }
  }
  
  # include communities data
  if (table %in% community_depending_queries) {
    data %<>%
      left_join(
        tradestatistics::ots_communities %>% 
          select(!!!syms(c("community_code", "community_name",
                           "community_color"))) %>% 
          distinct(), by = "community_code"
      )
    
    if (table == "yrpo") {
      data %<>%
        select(
          !!!syms(c(
            "year",
            "reporter_iso",
            "partner_iso",
            "reporter_fullname_english",
            "partner_fullname_english",
            "community_code",
            "community_name",
            "community_color"
          )),
          everything()
        )
    }
  }
  
  # order columns for consistent order
  data %<>%
    select(
      !!sym("year"),
      starts_with("reporter_"),
      starts_with("partner_"),
      starts_with("product_"),
      starts_with("group_"),
      starts_with("community_"),
      everything()
    )
  
  return(data)
}

#' Downloads and processes the data from the API to return a human-readable tibble (memoised, internal)
#' @description A composition of \code{ots_create_tidy_data_unmemoised()} and \code{memoise()} for caching the output
#' @importFrom memoise memoise
#' @keywords internal
ots_create_tidy_data_memoised <- memoise::memoise(ots_create_tidy_data_unmemoised)
