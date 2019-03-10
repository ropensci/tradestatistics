#' String matching of official product names and Harmonized System (HS) codes
#' according to the United Nations nomenclature
#' @description This function takes a text string and searches within the
#' package data for all matching product codes in the context of valid API
#' product codes.
#' @param productname A text string such as "Animals", "COPPER" or "fruits".
#' @param productgroup A text string such as "meat", "FISH" or "Dairy".
#' @return A tibble with all possible matches (no uppercase distinction)
#' showing the product name, product code and corresponding trade
#' classification (e.g. HS92 or SITC)
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter
#' @importFrom rlang sym
#' @importFrom stringr str_replace_all str_detect str_to_lower
#' @importFrom utils data
#' @export
#' @examples
#' ots_product_code(productname = "animals")
#' ots_product_code(productgroup = "fish")
#' ots_product_code(productname = "milk", productgroup = "dairy")
#' @keywords functions

ots_product_code <- function(productname = NULL, productgroup = NULL) {
  if (is.null(productname) & is.null(productgroup)) {
    d <- tradestatistics::ots_attributes_products
  }
  
  if (!is.null(productname) & is.null(productgroup)) {
    stopifnot(is.character(productname))
    stopifnot(nchar(productname) > 0)

    productname <- iconv(productname, to = "ASCII//TRANSLIT", sub = "")
    productname <- stringr::str_replace_all(productname, "[^[:alpha:]|[:space:]]", "")

    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    d <- tradestatistics::ots_attributes_products %>%
      dplyr::mutate(
        type_product = productname
      ) %>%
      dplyr::filter(
        stringr::str_detect(
          stringr::str_to_lower(!!sym("commodity_fullname_english")), productname
        )
      )
  }

  if (is.null(productname) & !is.null(productgroup)) {
    stopifnot(is.character(productgroup))
    stopifnot(nchar(productgroup) > 0)

    productgroup <- iconv(productgroup, to = "ASCII//TRANSLIT", sub = "")
    productgroup <- stringr::str_replace_all(productgroup, "[^[:alpha:]|[:space:]]", "")

    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    d <- tradestatistics::ots_attributes_products %>%
      dplyr::mutate(
        type_group = productgroup
      ) %>%
      dplyr::filter(
        stringr::str_detect(
          stringr::str_to_lower(!!sym("group_name")), productgroup
        )
      )
  }

  if (!is.null(productname) & !is.null(productgroup)) {
    stopifnot(is.character(productname))
    stopifnot(nchar(productname) > 0)

    stopifnot(is.character(productgroup))
    stopifnot(nchar(productgroup) > 0)

    productname <- iconv(productname, to = "ASCII//TRANSLIT", sub = "")
    productname <- stringr::str_replace_all(productname, "[^[:alpha:]|[:space:]]", "")

    productgroup <- iconv(productgroup, to = "ASCII//TRANSLIT", sub = "")
    productgroup <- stringr::str_replace_all(productgroup, "[^[:alpha:]|[:space:]]", "")

    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    d <- tradestatistics::ots_attributes_products %>%
      dplyr::mutate(
        type_name = productname,
        type_group = productgroup
      ) %>%
      dplyr::filter(
        stringr::str_detect(
          stringr::str_to_lower(!!sym("commodity_fullname_english")), productname
        ),
        stringr::str_detect(
          stringr::str_to_lower(!!sym("group_name")), productgroup
        )
      )
  }

  return(d)
}
