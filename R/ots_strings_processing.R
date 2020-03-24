#' String matching of official country names and ISO-3 codes according to
#' the United Nations nomenclature
#' @description Takes a text string and searches within the
#' package data for a country code in the context of valid API country codes.
#' @param countryname A text string such as "Chile", "CHILE" or "CHL".
#' @return A single character if there is a exact match (e.g.
#' \code{ots_country_code("Chile")}) or a tibble in case of multiple matches
#' (e.g. \code{ots_country_code("Germany")})
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate
#' @importFrom rlang sym
#' @importFrom stringr str_detect str_to_lower str_trim str_squish str_replace_all
#' @export
#' @examples
#' ots_country_code("Chile ")
#' ots_country_code("america")
#' ots_country_code("UNITED  STATES")
#' ots_country_code(" united_")
#' @keywords functions
ots_country_code <- function(countryname = NULL) {
  if (is.null(countryname)) {
    stop("'countryname' is NULL.")
  } else {
    stopifnot(is.character(countryname))
    
    countryname <- iconv(countryname, to = "ASCII//TRANSLIT", sub = " ")
    countryname <- str_replace_all(countryname, "[^[:alpha:]]", " ")
    countryname <- str_squish(countryname)
    countryname <- str_trim(countryname)
    countryname <- str_to_lower(countryname)
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

  if (countryname == "") {
    stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the countries table provided within this package.")
  } else {
    countrycode <- tradestatistics::ots_countries %>%
      filter(
        str_detect(
          str_to_lower(!!sym("country_fullname_english")), countryname
        )
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
#' showing the product name and product code
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
#' @keywords functions
ots_product_code <- function(productname = NULL, productgroup = NULL) {
  if (is.null(productname) & is.null(productgroup)) {
    stop("'productname' and 'productgroup' are NULL.")
  }
  
  if (!is.null(productname) & is.null(productgroup)) {
    stopifnot(is.character(productname))
    # stopifnot(nchar(productname) > 0)
    
    productname <- iconv(productname, to = "ASCII//TRANSLIT", sub = "")
    productname <- str_replace_all(productname, "[^[:alpha:]]", "")
    productname <- str_squish(productname)
    productname <- str_trim(productname)
    
    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    if (productname == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the products table provided within this package.")
    } else {
      d <- tradestatistics::ots_products %>%
        mutate(
          type_product = productname
        ) %>%
        filter(
          str_detect(
            str_to_lower(!!sym("product_fullname_english")), str_to_lower(!!sym("productname"))
          )
        )
    }
  }
  
  if (is.null(productname) & !is.null(productgroup)) {
    stopifnot(is.character(productgroup))
    # stopifnot(nchar(productgroup) > 0)
    
    productgroup <- iconv(productgroup, to = "ASCII//TRANSLIT", sub = "")
    productgroup <- str_replace_all(productgroup, "[^[:alpha:]]", "")
    productgroup <- str_squish(productgroup)
    productgroup <- str_trim(productgroup)
    
    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    if (productgroup == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the products table provided within this package.")
    } else {
      d <- tradestatistics::ots_products %>%
        mutate(
          type_group = productgroup
        ) %>%
        filter(
          str_detect(
            str_to_lower(!!sym("group_name")), str_to_lower(!!sym("productgroup"))
          )
        )
    }
  }
  
  if (!is.null(productname) & !is.null(productgroup)) {
    stopifnot(is.character(productname))
    # stopifnot(nchar(productname) > 0)
    
    stopifnot(is.character(productgroup))
    # stopifnot(nchar(productgroup) > 0)
    
    productname <- iconv(productname, to = "ASCII//TRANSLIT", sub = "")
    productname <- str_replace_all(productname, "[^[:alpha:]]", "")
    productname <- str_squish(productname)
    productname <- str_trim(productname)
    
    productgroup <- iconv(productgroup, to = "ASCII//TRANSLIT", sub = "")
    productgroup <- str_replace_all(productgroup, "[^[:alpha:]]", "")
    productgroup <- str_squish(productgroup)
    productgroup <- str_trim(productgroup)
    
    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    if (productname == "" | productgroup == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the products table provided within this package.")
    } else {
      d <- tradestatistics::ots_products %>%
        mutate(
          type_name = productname,
          type_group = productgroup
        ) %>%
        filter(
          str_detect(
            str_to_lower(!!sym("product_fullname_english")), str_to_lower(!!sym("productname"))
          ),
          str_detect(
            str_to_lower(!!sym("group_name")), str_to_lower(!!sym("productgroup"))
          )
        )
    }
  }
  
  return(d)
}

#' String matching of unofficial product section names and product section
#' codes
#' @description Takes a text string and searches within the
#' package data for all matching product communities in the context of valid API
#' product communities
#' @param productsection A text string such as "animals", or "FOODSTUFFS".
#' @return A tibble with all possible matches (no uppercase distinction)
#' showing the section name and section code
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter
#' @importFrom rlang sym
#' @importFrom stringr str_detect str_to_lower str_trim str_squish
#' @importFrom utils data
#' @export
#' @examples
#' ots_product_section(productsection = "  Animals")
#' ots_product_section(productsection = "FABRIC ")
#' @keywords functions
ots_product_section <- function(productsection = NULL) {
  if (is.null(productsection)) {
    stop("'productsection' is NULL.")
  }
  
  stopifnot(is.character(productsection))
  
  productsection <- iconv(productsection, to = "ASCII//TRANSLIT", sub = "")
  productsection <- str_replace_all(productsection, "[^[:alpha:]]", "")
  productsection <- str_squish(productsection)
  productsection <- str_trim(productsection)
  
  # get the products dataset, create the type_product column,
  # bind them all together and do the search
  if (productsection == "") {
    stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the products table provided within this package.")
  } else {
    d <- tradestatistics::ots_sections %>%
      mutate(
        type_section = productsection
      ) %>%
      filter(
        str_detect(
          str_to_lower(!!sym("section_fullname_english")), str_to_lower(!!sym("productsection"))
        )
      )
  }
  
  return(d)
}
