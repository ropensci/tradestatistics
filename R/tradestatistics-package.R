#' @keywords internal
"_PACKAGE"

utils::globalVariables(c(
  "year", "country_iso",
  "country_name_english", "country_fullname_english",
  "commodity_fullname_english", "section_fullname_english", "commodity_code", 
  "group_code", "trade_value_usd_exp", "trade_value_usd_imp",
  "trade_value_usd_top_exp", "trade_value_usd_top_imp",
  "gdp_deflator", "conversion_factor", "conversion_year", "year_from",
  "year_to", "observation", "..group", "..productname", "..section",
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
#' @format A data frame with 12 rows and 3 variables
#' \describe{
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
#' @format A data frame with 8,010 observations on the following 4 variables
#' \describe{
#'   \item{\code{year_from}}{Integer values in the range 1980-2020}
#'   \item{\code{year_to}}{Integer values in the range 1981-2021}
#'   \item{\code{country_iso}}{ISO code of the country (e.g. "chl" means Chile)}
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
#' @format A data frame with 275 observations on the following 5 variables
#' \describe{
#'   \item{\code{country_iso}}{ISO-3 code of the country (e.g. "deu" means Germany)}
#'   \item{\code{country_name_english}}{Country name (e.g. Germany)}
#'   \item{\code{country_fullname_english}}{Country name with indications (e.g. Germany as "Germany (former Federal Republic of Germany until 1990)")}
#'   \item{\code{continent_name_english}}{Continent where the country belongs to (e.g., Europe)}
#'   \item{\code{continent_id}}{Numeric id of the continent where the country belongs to (e.g., 5)}
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
#' @format A data frame with 275 rows and 3 variables
#' \describe{
#'   \item{\code{country_iso}}{ISO code of the country (e.g. "chl" means Chile)}
#'   \item{\code{continent_id}}{Numeric id of the continent}
#'   \item{\code{country_color}}{Country hex color (e.g. '#D05555')}
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
#' @format A data frame with 5,302 observations on the following 4 variables
#' \describe{
#'   \item{\code{commodity_code}}{HS six digits commodity code (e.g. 010110)}
#'   \item{\code{commodity_code_short}}{HS four digits commodity code (e.g. 0101)}
#'   \item{\code{commodity_fullname_english}}{HS six digits commodity name (e.g. 'Horses, asses, mules and hinnies; live, pure-bred breeding animals')}
#'   \item{\code{section_code}}{HS section code (e.g. '01')}
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
#' \describe{
#'   \item{\code{commodity_code}}{HS four digits commodity code (e.g. 0101)}
#'   \item{\code{commodity_fullname_english}}{HS four digits commodity names (e.g. 'Horses, asses, mules and hinnies; live')}
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
#' \describe{
#'   \item{\code{section_code}}{HS section code (e.g. '01')}
#'   \item{\code{section_fullname_english}}{HS section name (e.g. 'Live animals and animal products')}
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
#' \describe{
#'   \item{\code{section_code}}{HS section code (e.g. '01')}
#'   \item{\code{section_color}}{HS section color (e.g. '#74c0e2')}
#' }
NULL
