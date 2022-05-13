#' @keywords internal
"_PACKAGE"

utils::globalVariables(c(
  "year", "country_iso",
  "country_name_english", "country_fullname_english",
  "commodity_fullname_english", "section_fullname_english", "commodity_code", 
  "group_code", "trade_value_usd_exp", "trade_value_usd_imp",
  "trade_value_usd_top_exp", "trade_value_usd_top_imp",
  "gdp_deflator", "conversion_factor", "conversion_year", "from", "to", "observation",
  "..group", "..productname", "..section",
  "..reference_year", "..year", "..columns_order", "."
  ))

#' OTS Tables
#'
#' Existing API tables with both description and source.
#'
#' @docType data
#' @keywords datasets
#' @name ots_tables
#' @usage ots_tables
#' @source Open Trade Statistics
#' @format A data frame with 16 rows and 3 variables
#' \itemize{
#'   \item{\code{table}}{Table name}
#'   \item{\code{description}}{Description of table contents}
#'   \item{\code{source}}{Source for the data (OTS tables are processed after UN Comtrade raw data)}
#' }
NULL

#' GDP Deflator
#'
#' Year to year GDP deflator some of the countries in the OTS database. For 
#' countries not available in the World Bank database, rows labelled as "wld" 
#' are provided, which were computed as the weighted median for each year using 
#' the GDP of listed countries for each year expressed as constant dollars of 
#' the year 2010.
#'
#' @docType data
#' @keywords datasets
#' @name ots_gdp_deflator
#' @usage ots_gdp_deflator
#' @source Open Trade Statistics
#' @format A data frame with 7,238 observations on the following 4 variables
#' \itemize{
#'   \item{\code{country_iso}}{ISO code of the country (e.g. "chl" means Chile)}
#'   \item{\code{from}}{Integer values in the range 1980-2018}
#'   \item{\code{to}}{Integer values in the range 1981-2019}
#'   \item{\code{gdp_deflator}}{Numeric value expressed as one plus 1-year deflator}
#' }
NULL

#' OTS Countries
#'
#' Official country names, ISO-3 codes, continent and EU membership.
#'
#' @docType data
#' @keywords datasets
#' @name ots_countries
#' @usage ots_countries
#' @source Open Trade Statistics
#' @format A data frame with 264 observations on the following 5 variables
#' \itemize{
#'   \item{\code{country_iso}}{ISO code of the country (e.g. "chl" means Chile)}
#'   \item{\code{country_name_english}}{Country name (e.g. Germany)}
#'   \item{\code{country_fullname_english}}{Country name with indications (e.g. Germany (former Federal Republic of Germany until 1990))}
#'   \item{\code{continent_id}}{Numeric id of the continent where the country belongs to}
#'   \item{\code{continent_name_english}}{Continent where the country belongs to}
#' }
NULL

#' OTS Countries Colors
#'
#' Unofficial colors to ease visualization for countries.
#'
#' @docType data
#' @keywords datasets
#' @name ots_countries_colors
#' @usage ots_countries_colors
#' @source Open Trade Statistics
#' @format A data frame with 276 rows and 3 variables
#' \itemize{
#'   \item{\code{continent_id}}{Numeric id of the continent where the country belongs to}
#'   \item{\code{country_iso}}{ISO code of the country (e.g. "chl" means Chile)}
#'   \item{\code{country_color}}{Section hex color (e.g. '#D05555')}
#' }
NULL

#' OTS Commodities
#'
#' Official commodity names from the Harmonized System rev 2012
#' (HS12, six digits detail).
#'
#' @docType data
#' @keywords datasets
#' @name ots_commodities
#' @usage ots_commodities
#' @source Open Trade Statistics
#' @format A data frame with 5,304 observations on the following 4 variables
#' \itemize{
#'   \item{\code{commodity_code}}{Code of every commodity (e.g. 010110)}
#'   \item{\code{commodity_fullname_english}}{HS commodity names (e.g. 'Horses, asses, mules and hinnies; live, pure-bred breeding animals')}
#'   \item{\code{section_code}}{Section code (e.g. 01)}
#'   \item{\code{section_fullname_english }}{Section name (e.g. 'Live animals and animal products')}
#' }
NULL

#' OTS Commodities Short
#'
#' Official commodity names from the Harmonized System rev 2012
#' (HS12, four digits detail).
#'
#' @docType data
#' @keywords datasets
#' @name ots_commodities_short
#' @usage ots_commodities_short
#' @source Open Trade Statistics
#' @format A data frame with 1,225 observations on the following 2 variables
#' \itemize{
#'   \item{\code{commodity_code}}{Code of every commodity (e.g. 010110)}
#'   \item{\code{commodity_fullname_english}}{HS commodity names (e.g. 'Horses, asses, mules and hinnies; live, pure-bred breeding animals')}
#'   \item{\code{section_code}}{Section code (e.g. 01)}
#'   \item{\code{section_fullname_english }}{Section name (e.g. 'Live animals and animal products')}
#' }
NULL

#' OTS Sections
#'
#' Official section names from the Harmonized System rev 2012 (HS12).
#'
#' @docType data
#' @keywords datasets
#' @name ots_sections
#' @usage ots_sections
#' @source Adapted from UN COMTRADE
#' @format A data frame with 22 rows and 2 variables
#' \itemize{
#'   \item{\code{section_code}}{Section code (e.g. '01')}
#'   \item{\code{section_fullname_english}}{Section hex color (e.g. 'Live animals and animal products')}
#' }
NULL

#' OTS Sections Colors
#'
#' Unofficial colors to ease visualization for the sections in
#' the Harmonized System rev 2012 (HS12).
#'
#' @docType data
#' @keywords datasets
#' @name ots_sections_colors
#' @usage ots_sections_colors
#' @source Open Trade Statistics
#' @format A data frame with 22 rows and 2 variables
#' \itemize{
#'   \item{\code{section_code}}{Section code (e.g. '01')}
#'   \item{\code{section_color}}{Section hex color (e.g. '#74c0e2')}
#' }
NULL

#' OTS Distances
#'
#' Distance between countries, alongside colonial relation, common language,
#' and continuity.
#'
#' @docType data
#' @keywords datasets
#' @name ots_distances
#' @usage ots_distances
#' @source Adapted from CEPII
#' @format A data frame with 22,791 rows and 8 variables
#' \itemize{
#'   \item{\code{country1}}{First ISO-3 code in the dyad (alphabetical order)}
#'   \item{\code{country2}}{Second ISO-3 code in the dyad (alphabetical order)}
#'   \item{\code{dist}}{Distance between most populated cities (in kilometers)}
#'   \item{\code{distcap}}{Distance between capitals (in kilometers)}
#'   \item{\code{colony}}{Variable coded as 1 when the two countries are or were in a colonial relation}
#'   \item{\code{comlang_ethno}}{Variable coded as 1 when the two countries have at least 9\% of their population speaking the same language}
#'   \item{\code{comlang_off}}{Variable coded as 1 when the two countries share the same official language}
#'   \item{\code{contig}}{Variable coded as 1 when the two countries are next to each other and 0 otherwise}
#' }
NULL
