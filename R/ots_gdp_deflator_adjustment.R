#' Expresses tidy data from the API in dollars of a reference year
#' @description Uses GDP deflator records from The World Bank to
#' convert trade records and express them in dollars of the same year. The
#' records are internally subsetted to World's values, because country specific
#' levels would largely re-scale observations for reporters that reflect
#' unstable macroeconomic policies.
#' @param trade_data A tibble obtained by using ots_create_tidy_data.
#' Default set to \code{NULL}.
#' @param reference_year Year contained within the years specified in
#' api.tradestatistics.io/year_range (e.g. \code{2010}).
#' Default set to \code{NULL}.
#' @importFrom data.table `:=` rbindlist last
#' @export
#' @examples
#' \dontrun{
#' # The next example can take more than 5 seconds to compute,
#' # so this is shown without evaluation according to CRAN rules
#'
#' # Convert dollars of 2010 to dollars of 2000
#' d <- ots_create_tidy_data(years = 2010, reporters = "chl", partners = "chn")
#' ots_gdp_deflator_adjustment(trade_data = d, reference_year = 2000)
#' }
#' @keywords functions
ots_gdp_deflator_adjustment <- function(trade_data = NULL, reference_year = NULL) {
  # Check input -------------------------------------------------------------
  if (is.null(trade_data)) {
    stop("The input data cannot be NULL.")
  }

  if (is.null(reference_year)) {
    stop("The reference year cannot be NULL."
    )
  }

  ots_gdp_deflator_min_year <- min(tradestatistics::ots_gdp_deflator$year_from)
  ots_gdp_deflator_max_year <- max(tradestatistics::ots_gdp_deflator$year_from)

  if (!is.numeric(reference_year) |
    !(reference_year >= ots_gdp_deflator_min_year &
      reference_year <= ots_gdp_deflator_max_year)) {
    stop(sprintf("The reference year must be numeric and contained within ots_gdp_deflator years range that is %s-%s.",
        ots_gdp_deflator_min_year,
        ots_gdp_deflator_max_year
      )
    )
  }

  # Filter year conversion rates and join data ------------------------------
  years <- unique(trade_data$year)
  
  d1 <- lapply(
    years,
    function(year) {
      if (year < reference_year) {
        tradestatistics::ots_gdp_deflator[year_to <= reference_year &
          year_to > year & country_iso == "wld",
          .(gdp_deflator = last(cumprod(gdp_deflator)))][,
          `:=`(year = ..year, conversion_year = ..reference_year)][,
          .(year, conversion_year, gdp_deflator)]
      } else if (year > reference_year) {
        tradestatistics::ots_gdp_deflator[year_from >= reference_year &
          year_from < year &
          country_iso == "wld",
          .(gdp_deflator = 1/last(cumprod(gdp_deflator)))][,
          `:=`(year = ..year, conversion_year = ..reference_year)][,
          .(year, conversion_year, gdp_deflator)]
      } else if (year == reference_year) {
        data.frame(
          year = year, conversion_year = year, gdp_deflator = 1
        )
      }
    }
  )
  d1 <- rbindlist(d1)

  d2 <- trade_data[d1, on = .(year), allow.cartesian = TRUE][,
    `:=`(trade_value_usd_exp = round(trade_value_usd_exp * gdp_deflator, 0),
         trade_value_usd_imp = round(trade_value_usd_imp * gdp_deflator, 0))]

  return(d2)
}
