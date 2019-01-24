#' Obtain a table of official country names and ISO-3 codes according to
#' the United Nations nomenclature
#' @description This function takes a text string and searches within the
#' package data for a country code in the context of valid API country codes.
#' @param countryname A text string such as "Chile", "CHILE" or "CHL".
#' @return A single character if there is a exact match (e.g.
#' \code{ots_country_code("Chile")}) or a tibble in case of multiple matches
#' (e.g. \code{ots_country_code("Germany")})
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter
#' @importFrom rlang sym
#' @importFrom purrr as_vector
#' @importFrom stringr str_detect str_to_lower
#' @export
#' @examples
#' # Single match with no replacement
#' ots_country_code("Chile")
#' 
#' # Single match with replacement
#' ots_country_code("America")
#' 
#' # Double match with no replacement
#' ots_country_code("Germany")
#' @keywords functions

ots_country_code <- function(countryname = NULL) {
  countryname <- str_to_lower(countryname)

  countryname <- switch(countryname,
    "us" = "usa",
    "america" = "usa",
    "united states of america" = "usa",
    "uk" = "united kingdom",
    "england" = "united kingdom",
    "scotland" = "united kingdom",
    "holland" = "netherlands",
    "ussr" = "russia",
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

  countrycode <- tradestatistics::ots_attributes_countries %>%
    filter(
      str_detect(
        str_to_lower(!!sym("country_fullname_english")), countryname
      )
    ) %>%
    select(!!sym("country_iso")) %>%
    as_vector()

  if (length(countrycode) == 0) {
    message(
      "There is no match for your search. Please check the spelling or
       explore the countries table provided within this package."
    )
  }

  if (length(countrycode) > 1) {
    message(
      "There is more than one match for your search. Please try again 
       using one of these codes:"
    )

    f <- tradestatistics::ots_attributes_countries %>%
      filter(
        str_detect(
          str_to_lower(!!sym("country_name_english")),
          str_to_lower(countryname)
        )
      )

    if (countryname == "all") {
      f <- filter(f, !country_iso %in% c("mhl", "wlf"))
    }

    return(f)
  } else {
    return(countrycode)
  }
}
