#' String matching of official country names and ISO-3 codes according to
#' the United Nations nomenclature
#' @description This function takes a text string and searches within the
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
      countryname can't have zero characters after removing numbers,
      special symbols and multiple spaces.
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

  if (length(countrycode) == 0) {
    message(
      "There is no match for your search. Please check the spelling or
       explore the countries table provided within this package."
    )
  }

  return(countrycode)
}
