#' String matching of official country names and ISO-3 codes according to
#' the United Nations nomenclature
#' @description Takes a text string and searches within the
#' package data for a country code in the context of valid API country codes.
#' @param countryname A text string such as "Chile", "CHILE" or "CHL".
#' @return A single character if there is a exact match (e.g.
#' \code{ots_country_code("Chile")}) or a tibble in case of multiple matches
#' (e.g. \code{ots_country_code("Germany")})
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @importFrom purrr map_chr
#' @importFrom stringr str_detect str_to_lower str_trim str_squish
#' @export
#' @examples
#' ots_country_code("Chile ")
#' ots_country_code("america")
#' ots_country_code("UNITED  STATES")
#' ots_country_code(" united_")
#' @keywords functions
ots_country_code <- function(countryname = NULL) {
  if (is.null(countryname)) {
    stop(
      "
      countryname can't be NULL
      Try with a quoted text string (e.g. ots_country_code(\"chi\")).
      "
    )
  } else {
    countryname <- iconv(countryname, to = "ASCII//TRANSLIT", sub = " ")
    countryname <- stringr::str_replace_all(countryname, "[^[:alpha:]]", " ")
    countryname <- stringr::str_squish(countryname)
    countryname <- stringr::str_trim(countryname)
  }

  if (any(nchar(countryname) < 1)) {
    stop(
      "
      countryname can't have zero characters after removing numbers, special symbols and multiple spaces.
      Try with a quoted text string (e.g. ots_country_code(\"chi\")).
      "
    )
  } else {
    countryname <- stringr::str_to_lower(countryname)
  }

  countryname <- switch(
    countryname,
    "us" = "usa",
    "america" = "usa",
    "united states" = "usa",
    "united states of america" = "usa",
    "uk" = "united kingdom",
    "england" = "united kingdom",
    "scotland" = "united kingdom",
    "holland" = "netherlands",
    "myanmar" = "burma",
    "persia" = "iran",
    "siam" = "thailand",
    "indochina" = "vietnam",
    "rhodesia" = "zimbabwe",
    "british honduras" = "belice",
    "bengal" = "bangladesh",
    "east pakistan" = "bangladesh",
    "zaire" = "democratic republic of the congo",
    countryname
  )

  countrycode <- tradestatistics::ots_countries %>%
    dplyr::filter(
      stringr::str_detect(
        stringr::str_to_lower(!!sym("country_fullname_english")), countryname
      )
    )

  if (nrow(countrycode) == 0) {
    stop(
      "
      There is no match for your search.
      Please check the spelling or explore the countries table provided within this package.
      "
    )
  }

  return(countrycode)
}

#' String matching of official product names and Harmonized System (HS) codes
#' according to the United Nations nomenclature
#' @description Takes a text string and searches within the
#' package data for all matching product codes in the context of valid API
#' product codes.
#' @param productname A text string such as "Animals", "COPPER" or "fruits".
#' @param productgroup A text string such as "meat", "FISH" or "Dairy".
#' @return A tibble with all possible matches (no uppercase distinction)
#' showing the product name, product code and corresponding trade
#' classification (e.g. HS92 or SITC)
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter
#' @importFrom rlang sym
#' @importFrom stringr str_detect str_to_lower str_trim str_squish
#' @importFrom utils data
#' @export
#' @examples
#' ots_product_code(productname = "ANIMALS ")
#' ots_product_code(productgroup = "  fish")
#' ots_product_code(productname = "Milk", productgroup = "Dairy")
#' ots_product_code()
#' @keywords functions
ots_product_code <- function(productname = NULL, productgroup = NULL) {
  if (is.null(productname) & is.null(productgroup)) {
    d <- tradestatistics::ots_products
  }
  
  if (!is.null(productname) & is.null(productgroup)) {
    stopifnot(is.character(productname))
    stopifnot(nchar(productname) > 0)
    
    productname <- iconv(productname, to = "ASCII//TRANSLIT", sub = " ")
    productname <- stringr::str_replace_all(productname, "[^[:alpha:]]", " ")
    productname <- stringr::str_squish(productname)
    productname <- stringr::str_trim(productname)
    
    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    d <- tradestatistics::ots_products %>%
      dplyr::mutate(
        type_product = productname
      ) %>%
      dplyr::filter(
        stringr::str_detect(
          stringr::str_to_lower(!!sym("product_fullname_english")), productname
        )
      )
  }
  
  if (is.null(productname) & !is.null(productgroup)) {
    stopifnot(is.character(productgroup))
    stopifnot(nchar(productgroup) > 0)
    
    productgroup <- iconv(productgroup, to = "ASCII//TRANSLIT", sub = " ")
    productgroup <- stringr::str_replace_all(productgroup, "[^[:alpha:]]", " ")
    productgroup <- stringr::str_squish(productgroup)
    productgroup <- stringr::str_trim(productgroup)
    
    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    d <- tradestatistics::ots_products %>%
      dplyr::mutate(
        type_group = productgroup
      ) %>%
      dplyr::filter(
        stringr::str_detect(
          stringr::str_to_lower(!!sym("group_name")), productgroup
        )
      )
  }
  
  if (!is.null(productname) & !is.null(productgroup)) {
    stopifnot(is.character(productname))
    stopifnot(nchar(productname) > 0)
    
    stopifnot(is.character(productgroup))
    stopifnot(nchar(productgroup) > 0)
    
    productname <- iconv(productname, to = "ASCII//TRANSLIT", sub = " ")
    productname <- stringr::str_replace_all(productname, "[^[:alpha:]]", " ")
    productname <- stringr::str_squish(productname)
    productname <- stringr::str_trim(productname)
    
    productgroup <- iconv(productgroup, to = "ASCII//TRANSLIT", sub = " ")
    productgroup <- stringr::str_replace_all(productgroup, "[^[:alpha:]]", " ")
    productgroup <- stringr::str_squish(productgroup)
    productgroup <- stringr::str_trim(productgroup)
    
    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    d <- tradestatistics::ots_products %>%
      dplyr::mutate(
        type_name = productname,
        type_group = productgroup
      ) %>%
      dplyr::filter(
        stringr::str_detect(
          stringr::str_to_lower(!!sym("product_fullname_english")), productname
        ),
        stringr::str_detect(
          stringr::str_to_lower(!!sym("group_name")), productgroup
        )
      )
  }
  
  return(d)
}

