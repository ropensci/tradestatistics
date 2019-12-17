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
#' @param products HS codes (e.g. \code{"0101"}, \code{"01"} or search matches for \code{"apple"})
#' to filter products. Default set to \code{"all"}.
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
#' @importFrom purrr map_df as_vector
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
ots_create_tidy_data <- function(years = 1962,
                                 reporters = "all",
                                 partners = "all",
                                 products = "all",
                                 table = "yrpc",
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
  
  ots_with_cache(
    use_cache = use_cache, file = file,
    memoised = ots_create_tidy_data_memoised,
    unmemoised = ots_create_tidy_data_unmemoised,
    years = years,
    reporters = reporters,
    partners = partners,
    products = products,
    table = table,
    max_attempts = max_attempts,
    include_shortnames = include_shortnames,
    include_communities = include_communities,
    use_localhost = use_localhost
  )
}

#' Downloads and processes the data from the API to return a human-readable tibble (unmemoised, internal)
#' @description A separation of the downloading part in \code{ots_create_tidy_data()} for making caching optional.
#' @export
#' @keywords internal
ots_create_tidy_data_unmemoised <- function(years,
                                            reporters,
                                            partners,
                                            products,
                                            table,
                                            max_attempts,
                                            include_shortnames,
                                            include_communities,
                                            use_localhost) {
  # Check tables ----
  if (!table %in% tradestatistics::ots_tables$table) {
    stop("The requested table does not exist. Please check the spelling or\nexplore the 'ots_table' table provided within this package.")
  }

  # Check years ----
  year_depending_queries <- grep("^reporters|rankings$|^y",
    tradestatistics::ots_tables$table,
    value = T
  )

  url <- "year_range"

  resp <- crul::HttpClient$new(url = "https://api.tradestatistics.io/")
  resp <- resp$get(url)

  year_range <- purrr::as_vector(jsonlite::fromJSON(resp$parse(encoding = "UTF-8")))

  if (all(years %in% min(year_range):max(year_range)) != TRUE &
    table %in% year_depending_queries) {
    stop("Provided that the table you requested contains a 'year' field, please\nverify that you are requesting data contained within the years exposed\nin api.tradestatistics.io/year_range.")
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
      
      if (length(reporters_no_iso) > 0) {
        warning(
          sub(",([^,]*)$"," &\\1",
              sprintf("You wrote reporter strings that could not be matched to ISO codes.\nThese strings shall be ignored. Please check the spelling for:\n%s", paste0(sprintf("'%s'", reporters_no_iso), collapse = ", "))
          )
        )
      }
      
      reporters <- reporters_iso
      
      if (length(reporters) == 0) {
        stop("After ignoring the unmatched reporter strings, there were zero obtained\nISO codes and no data will be downloaded.")
      }
    }
  }

  if (!is.null(partners)) {
    if (!all(partners %in% tradestatistics::ots_countries$country_iso) == TRUE & table %in% partner_depending_queries) {
      partners_iso <- partners[partners %in% tradestatistics::ots_countries$country_iso]
      partners_no_iso <- partners[!partners %in% tradestatistics::ots_countries$country_iso]

      if (length(partners_no_iso) > 0) {
        warning(
          sub(",([^,]*)$"," &\\1",
              sprintf("You wrote partner strings that could not be matched to ISO codes.\nThese strings shall be ignored. Please check the spelling for:\n%s", paste0(sprintf("'%s'", partners_no_iso), collapse = ", "))
          )
        )
      }

      partners <- partners_iso
      
      if (length(partners) == 0) {
        stop("After ignoring the unmatched partner strings, there were zero obtained\nISO codes and no data will be downloaded.")
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
    pnm <- purrr::map_df(
      .x = seq_along(products_wm),
      ~ tradestatistics::ots_product_code(productname = products_wm[.x])
    )

    # group name match (gnm)
    gnm <- purrr::map_df(
      .x = seq_along(products_wm),
      ~ tradestatistics::ots_product_code(productgroup = products_wm[.x])
    )

    products_wm <- bind_rows(pnm, gnm) %>%
      distinct(!!sym("product_code")) %>%
      as_vector()

    products <- c(products[products %in%
      tradestatistics::ots_products$product_code], products_wm)
  }

  if (!all(as.character(products) %in%
    tradestatistics::ots_products$product_code == TRUE) &
    table %in% product_depending_queries) {
    stop("The requested products do not exist. Please check the spelling or\nexplore the 'ots_products' table provided within this package.")
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
    warning("The products argument will be ignored provided that you requested a\ntable without product_code field.")
  }

  if (is.null(reporters)) {
    reporters <- ""
  }

  if (is.null(partners)) {
    partners <- ""
  }

  condensed_parameters <- expand.grid(
    year = years,
    reporter = reporters,
    partner = partners,
    product = products,
    stringsAsFactors = FALSE
  )

  data <- purrr::map_df(
    .x = seq_len(nrow(condensed_parameters)),
    ~ ots_read_from_api(
      table = table,
      max_attempts = max_attempts,
      year = condensed_parameters$year[.x],
      reporter = condensed_parameters$reporter[.x],
      partner = condensed_parameters$partner[.x],
      product_code = condensed_parameters$product[.x],
      use_localhost = use_localhost
    )
  ) %>%
    dplyr::as_tibble()

  # no data in API message
  if (nrow(data) == 0) {
    stop("No data available. Try changing years, reporters, partners or products.")
  }

  # Add attributes based on codes, etc (and join years, if applicable) ------

  # include countries data
  tables_with_reporter <- c("yrpc", "yrp", "yrc", "yr")

  if (table %in% tables_with_reporter) {
    if (table %in% tables_with_reporter[1:2]) {
      data %<>%
        dplyr::left_join(dplyr::select(
          tradestatistics::ots_countries,
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
      data %<>%
        dplyr::left_join(dplyr::select(
          tradestatistics::ots_countries,
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
    data %<>%
      dplyr::left_join(dplyr::select(
        tradestatistics::ots_countries,
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
    data %<>%
      dplyr::left_join(tradestatistics::ots_products, by = "product_code")

    if (table == "yrpc") {
      data %<>%
        dplyr::select(
          !!!rlang::syms(c(
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
          dplyr::everything()
        )
    }

    if (table == "yrc") {
      data %<>%
        dplyr::select(
          !!!rlang::syms(c(
            "year",
            "reporter_iso",
            "reporter_fullname_english",
            "product_code",
            "product_fullname_english",
            "group_code",
            "group_name"
          )),
          dplyr::everything()
        )
    }

    if (table == "yc") {
      data %<>%
        dplyr::select(
          !!!rlang::syms(c(
            "year",
            "product_code",
            "product_fullname_english",
            "group_code",
            "group_name"
          )),
          dplyr::everything()
        )
    }
  }

  if (table %in% product_depending_queries & include_shortnames == TRUE) {
    data %<>%
      dplyr::left_join(tradestatistics::ots_product_shortnames) %>%
      dplyr::select(
        !!!rlang::sym(("year")),
        dplyr::starts_with("product_"),
        dplyr::starts_with("group_"),
        dplyr::everything()
      )
  }

  if (table %in% product_depending_queries & include_communities == TRUE) {
    data %<>%
      dplyr::left_join(tradestatistics::ots_communities) %>%
      dplyr::select(
        !!!rlang::sym(("year")),
        dplyr::starts_with("product_"),
        dplyr::starts_with("group_"),
        dplyr::starts_with("community_"),
        dplyr::everything()
      )
  }

  return(data)
}

#' Downloads and processes the data from the API to return a human-readable tibble (memoised, internal)
#' @description A composition of \code{ots_create_tidy_data_unmemoised()} and \code{memoise()} for caching the output
#' @export
#' @keywords internal
ots_create_tidy_data_memoised <- memoise::memoise(ots_create_tidy_data_unmemoised)
