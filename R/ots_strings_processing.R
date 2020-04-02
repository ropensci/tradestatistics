#' String matching of official country names and ISO-3 codes according to
#' the United Nations nomenclature
#' @description Takes a text string and searches within the
#' package data for a country code in the context of valid API country codes.
#' @param countryname A text string such as "Chile", "CHILE" or "CHL".
#' @return A single character if there is a exact match (e.g.
#' \code{ots_country_code("Chile")}) or a tibble in case of multiple matches
#' (e.g. \code{ots_country_code("Germany")})
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
    countryname <- gsub("[^[:alpha:]]", "", countryname)
    countryname <- tolower(countryname)
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
    countrycode <- tradestatistics::ots_countries[grepl(countryname, tolower(country_fullname_english))]
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
#' @importFrom data.table `:=`
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
    productname <- gsub("[^[:alpha:]]", "", productname)
    
    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    if (productname == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the products table provided within this package.")
    } else {
      d <- tradestatistics::ots_products[,
        `:=`(type_product = ..productname)][grepl(tolower(productname),
        tolower(product_fullname_english))]
    }
  }
  
  if (is.null(productname) & !is.null(productgroup)) {
    stopifnot(is.character(productgroup))
    # stopifnot(nchar(productgroup) > 0)
    
    productgroup <- iconv(productgroup, to = "ASCII//TRANSLIT", sub = "")
    productgroup <- gsub("[^[:alpha:]]", "", productgroup)
    
    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    if (productgroup == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the products table provided within this package.")
    } else {
      d <- tradestatistics::ots_groups[,
        `:=`(type_group = ..productgroup)][grepl(tolower(productgroup),
        tolower(group_fullname_english))]
    }
  }
  
  if (!is.null(productname) & !is.null(productgroup)) {
    stopifnot(is.character(productname))
    # stopifnot(nchar(productname) > 0)
    
    stopifnot(is.character(productgroup))
    # stopifnot(nchar(productgroup) > 0)
    
    productname <- iconv(productname, to = "ASCII//TRANSLIT", sub = "")
    productname <- gsub("[^[:alpha:]]", "", productname)
    
    productgroup <- iconv(productgroup, to = "ASCII//TRANSLIT", sub = "")
    productgroup <- gsub("[^[:alpha:]]", "", productgroup)

    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    if (productname == "" | productgroup == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the products table provided within this package.")
    } else {
      d <- tradestatistics::ots_groups[tradestatistics::ots_products[,
        `:=`(group_code = substr(product_code, 1, 2))],
        on = .(group_code), allow.cartesian = TRUE][,
        `:=`(type_name = ..productname,
        type_group = ..productgroup)][grepl(tolower(productname),
        tolower(product_fullname_english)) & grepl(tolower(productgroup),
        tolower(group_fullname_english))]
    }
  }
  
  return(d)
}

#' String matching of official product section names and product section
#' codes
#' @description Takes a text string and searches within the
#' package data for all matching product communities in the context of valid API
#' product communities
#' @param productsection A text string such as "animals", or "FOODSTUFFS".
#' @return A tibble with all possible matches (no uppercase distinction)
#' showing the section name and section code
#' @importFrom data.table `:=`
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
  productsection <- gsub("[^[:alpha:]]", "", productsection)

  # get the products dataset, create the type_product column,
  # bind them all together and do the search
  if (productsection == "") {
    stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the products table provided within this package.")
  } else {
    d <- tradestatistics::ots_sections_names[tradestatistics::ots_sections,
      on = .(section_code), allow.cartesian = TRUE][,
      `:=`(type_section = ..productsection)][grepl(tolower(productsection), 
      tolower(section_fullname_english))]
  }
  
  return(d)
}
