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

#' String matching of official commodity/group names and Harmonized System (HS) codes
#' according to the United Nations nomenclature
#' @description Takes a text string and searches within the
#' package data for all matching commodity codes in the context of valid API
#' commodity codes.
#' @param commodity A text string such as "Animals", "COPPER" or "fruits".
#' @param group A text string such as "meat", "FISH" or "Dairy".
#' @return A tibble with all possible matches (no uppercase distinction)
#' showing the commodity name and commodity code
#' @export
#' @examples
#' ots_commodity_code(commodity = "ANIMALS ")
#' ots_commodity_code(group = "  fish")
#' ots_commodity_code(commodity = "Milk", group = "Dairy")
#' @keywords functions
ots_commodity_code <- function(commodity = NULL, group = NULL) {
  if (is.null(commodity) & is.null(group)) {
    stop("'commodity' and 'group' are NULL.")
  }
  
  if (!is.null(commodity) & is.null(group)) {
    stopifnot(is.character(commodity))
    # stopifnot(nchar(commodity) > 0)
    
    commodity <- tolower(iconv(commodity, to = "ASCII//TRANSLIT", sub = ""))
    commodity <- gsub("[^[:alpha:]]", "", commodity)

    if (commodity == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the commodities table provided within this package.")
    } else {
      d <- tradestatistics::ots_commodities[grepl(commodity, tolower(commodity_fullname_english))]
    }
  }
  
  if (is.null(commodity) & !is.null(group)) {
    stopifnot(is.character(group))
    
    group <- tolower(iconv(group, to = "ASCII//TRANSLIT", sub = ""))
    group <- gsub("[^[:alpha:]]", "", group)
    
    if (group == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the commodities table provided within this package.")
    } else {
      dg <- unique(tradestatistics::ots_commodities[, c("group_code", "group_fullname_english")])
      
      d <- dg[grepl(group, tolower(group_fullname_english))]
    }
  }
  
  if (!is.null(commodity) & !is.null(group)) {
    stopifnot(is.character(commodity))
    # stopifnot(nchar(commodity) > 0)
    
    stopifnot(is.character(group))
    # stopifnot(nchar(group) > 0)
    
    commodity <- tolower(iconv(commodity, to = "ASCII//TRANSLIT", sub = ""))
    commodity <- gsub("[^[:alpha:]]", "", commodity)
    
    group <- tolower(iconv(group, to = "ASCII//TRANSLIT", sub = ""))
    group <- gsub("[^[:alpha:]]", "", group)

    if (commodity == "" | group == "") {
      stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the commodities table provided within this package.")
    } else {
      d <- tradestatistics::ots_commodities[
              grepl(commodity, tolower(commodity_fullname_english)) &
              grepl(group, tolower(group_fullname_english))]
    }
  }
  
  return(d)
}

#' String matching of official commodity section names and commodity community
#' codes
#' @description Takes a text string and searches within the
#' package data for all matching communities in the context of valid API
#' communities
#' @param community A text string such as "animals", or "FOODSTUFFS".
#' @return A tibble with all possible matches (no uppercase distinction)
#' showing the section name and section code
#' @export
#' @examples
#' ots_commodity_community(community = "Animal")
#' ots_commodity_community(community = "  Animals")
#' ots_commodity_community(community = "FABRIC ")
#' @keywords functions
ots_commodity_community <- function(community = NULL) {
  if (is.null(community)) {
    stop("'community' is NULL.")
  }
  
  stopifnot(is.character(community))
  
  community <- tolower(iconv(community, to = "ASCII//TRANSLIT", sub = ""))
  community <- gsub("[^[:alpha:]]", "", community)

  if (community == "") {
    stop("The input results in an empty string after removing multiple spaces and special symbols. Please check the spelling or explore the commodities table provided within this package.")
  } else {
    dc <- unique(tradestatistics::ots_communities[, c("community_code", "community_name")])
    d <- dc[grepl(community, tolower(community_name))]
  }
  
  return(d)
}
