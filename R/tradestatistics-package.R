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
#' A table describing existing API tables with both description and source.
#' This data is used by the functions provided within this package to validate
#' user parameters.
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
#' A table with the World's weigthed GDP deflator inflation since 2000.
#' Provides year to year GDP deflator values for every country listed in the 
#' World Bank database. This data is ready to be applied as a conversion rate 
#' to express dollars of year Y1 as dollars of year Y2.
#' For countries not listed in the World Bank database, rows labelled as "wld" 
#' are available, which were computed as the weighted median for each year using 
#' the GDP of listed countries for each year expressed as constant dollars of 
#' the year 2010.
#' This dataset is provided to be used with \code{ots_gdp_deflator_adjustment} 
#' that converts units forwards and backwards in time.
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
#' A table of official country names, ISO-3 codes and other metadata. The
#' availability is limited to countries with records in the OTS database.
#' This data is used by the functions provided within this package to complement
#' the data obtained from the API.
#'
#' @docType data
#' @keywords datasets
#' @name ots_countries
#' @usage ots_countries
#' @source Open Trade Statistics
#' @format A data frame with 264 observations on the following 6 variables
#' \itemize{
#'   \item{\code{country_iso}}{ISO code of the country (e.g. "chl" means Chile)}
#'   \item{\code{country_name_english}}{Country name (e.g. Germany)}
#'   \item{\code{country_fullname_english}}{Country name with indications (e.g. Germany (former Federal Republic of Germany until 1990))}
#'   \item{\code{continent_id}}{Numeric id of the continent where the country belongs to}
#'   \item{\code{continent_name_english}}{Continent where the country belongs to}
#'   \item{\code{eu28_member}}{Dummy variable such that 1 means "belongs to EU-28 group" and 0 otherwise}
#' }
NULL

#' OTS Commodities
#'
#' A table of official commodity names from the Harmonized System rev 2012
#' (HS12, also known as H4). Provides official commodity and section codes and 
#' names at six (6) digits detail taken from the United Nations official 
#' sources.
#' This data is used by the functions provided within this package to complement
#' the data obtained from the API.
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
#' A table of official commodity names from the Harmonized System rev 2012
#' (HS12, also known as H4). Provides official commodity and section codes and 
#' names at four (4) digits of detail taken from the United Nations official 
#' sources.
#' This data is used by the functions provided within this package to complement
#' the data obtained from the API.
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
#' A table of official section names from the Harmonized System rev 2012 
#' (HS12, also known as H4).
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
#' A table of official unofficial colors to ease visualization for the sections
#' the Harmonized System rev 2012 (HS12, also known as H4).
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
