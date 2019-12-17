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
  d1 <- purrr::map_df(
    unique(trade_data$year),
    function(year) {
      if (year <= reference_year) {
        tradestatistics::ots_inflation %>%
          dplyr::filter(
            !!sym("to") <= reference_year,
            !!sym("to") > year
          ) %>%
          dplyr::summarise(
            conversion_factor = dplyr::last(cumprod(!!sym("conversion_factor")))
          ) %>%
          dplyr::mutate(
            year = year,
            conversion_year = reference_year
          ) %>%
          dplyr::select(!!!rlang::syms(c("year", "conversion_year", "conversion_factor")))
      } else {
        tradestatistics::ots_inflation %>%
          dplyr::filter(
            !!sym("from") >= reference_year,
            !!sym("from") < year
          ) %>%
          dplyr::summarise(
            conversion_factor = 1 / dplyr::last(cumprod(!!sym("conversion_factor")))
          ) %>%
          dplyr::mutate(
            year = year,
            conversion_year = reference_year
          ) %>%
          dplyr::select(!!!rlang::syms(c("year", "conversion_year", "conversion_factor")))
      }
    }
  )

  d2 <- trade_data %>%
    dplyr::left_join(d1, by = "year") %>%
    dplyr::mutate(
      export_value_usd = !!sym("export_value_usd") * !!sym("conversion_factor"),
      import_value_usd = !!sym("import_value_usd") * !!sym("conversion_factor")
    ) %>%
    dplyr::select(-dplyr::matches("change"))

  return(d2)
}
