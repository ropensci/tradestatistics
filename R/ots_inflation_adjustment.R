#' Expresses tidy data from the API in dollars of a reference year
#' @description Uses inflation records from The World Bank to
#' convert trade records and express them in dollars of the same year.
#' @param trade_data A tibble obtained by using ots_create_tidy_data.
#' Default set to \code{NULL}.
#' @param reference_year Year contained within the years specified in
#' api.tradestatistics.io/year_range (e.g. \code{2010}).
#' Default set to \code{NULL}.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter summarise mutate distinct matches last
#' @importFrom rlang sym syms
#' @importFrom purrr map_df
#' @export
#' @examples
#' \dontrun{
#' # The next example can take more than 5 seconds to compute,
#' # so this is shown without evaluation according to CRAN rules
#'
#' # Convert dollars of 1980 to dollars of 2010
#' d <- ots_create_tidy_data(years = 1980, reporters = "chl", partners = "chn")
#' ots_inflation_adjustment(trade_data = d, reference_year = 2010)
#' }
#' @keywords functions
ots_inflation_adjustment <- function(trade_data = NULL, reference_year = NULL) {
  # Check input -------------------------------------------------------------
  if (is.null(trade_data)) {
    stop(
      "
      The input data cannot be null.
      "
    )
  }

  if (is.null(reference_year)) {
    stop(
      "
      The reference year cannot be null.
      "
    )
  }

  ots_inflation_min_year <- min(tradestatistics::ots_inflation$from)
  ots_inflation_max_year <- max(tradestatistics::ots_inflation$from)

  if (!is.numeric(reference_year) |
    !(reference_year >= ots_inflation_min_year &
      reference_year <= ots_inflation_max_year)) {
    stop(
      sprintf(
        "
      The reference year must be numeric and contained within ots_inflation years range
      that is %s-%s.
      ",
        ots_inflation_min_year,
        ots_inflation_max_year
      )
    )
  }

  # Filter year conversion rates and join data ------------------------------
  years <- unique(trade_data$year)
  
  d1 <- map_df(
    unique(trade_data$year),
    function(year) {
      if (year <= reference_year) {
        tradestatistics::ots_inflation %>%
          filter(
            !!sym("to") <= reference_year,
            !!sym("to") > year
          ) %>%
          summarise(
            conversion_factor = last(cumprod(!!sym("conversion_factor")))
          ) %>%
          mutate(
            year = year,
            conversion_year = reference_year
          ) %>%
          select(!!!syms(c("year", "conversion_year", "conversion_factor")))
      } else {
        tradestatistics::ots_inflation %>%
          filter(
            !!sym("from") >= reference_year,
            !!sym("from") < year
          ) %>%
          summarise(
            conversion_factor = 1 / last(cumprod(!!sym("conversion_factor")))
          ) %>%
          mutate(
            year = year,
            conversion_year = reference_year
          ) %>%
          select(!!!syms(c("year", "conversion_year", "conversion_factor")))
      }
    }
  )
  
  d1 <- d1 %>% 
    mutate(
      conversion_factor = ifelse(!!sym("year") == !!sym("conversion_year"), 1, !!sym("conversion_year"))
    )

  d2 <- trade_data %>%
    left_join(d1, by = "year") %>%
    mutate(
      export_value_usd = !!sym("export_value_usd") * !!sym("conversion_factor"),
      import_value_usd = !!sym("import_value_usd") * !!sym("conversion_factor")
    ) %>%
    select(-matches("change"))

  return(d2)
}
