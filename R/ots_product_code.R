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
#' @importFrom stringr str_detect str_to_lower str_trim str_squish
#' @importFrom utils data
#' @export
#' @examples
#' ots_product_code(productname = "ANIMALS ")
#' ots_product_code(productgroup = "  fish")
#' ots_product_code(productname = "Milk", productgroup = "Dairy")
#' ots_product_code()
#' @keywords functions

ots_product_code <- function(productname = NULL, productgroup = NULL) {
  if (is.null(productname) & is.null(productgroup)) {
    d <- tradestatistics::ots_products
  }

  if (!is.null(productname) & is.null(productgroup)) {
    stopifnot(is.character(productname))
    stopifnot(nchar(productname) > 0)

    productname <- iconv(productname, to = "ASCII//TRANSLIT", sub = " ")
    productname <- stringr::str_replace_all(productname, "[^[:alpha:]]", " ")
    productname <- stringr::str_squish(productname)
    productname <- stringr::str_trim(productname)

    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    d <- tradestatistics::ots_products %>%
      dplyr::mutate(
        type_product = productname
      ) %>%
      dplyr::filter(
        stringr::str_detect(
          stringr::str_to_lower(!!sym("product_fullname_english")), productname
        )
      )
  }

  if (is.null(productname) & !is.null(productgroup)) {
    stopifnot(is.character(productgroup))
    stopifnot(nchar(productgroup) > 0)

    productgroup <- iconv(productgroup, to = "ASCII//TRANSLIT", sub = " ")
    productgroup <- stringr::str_replace_all(productgroup, "[^[:alpha:]]", " ")
    productgroup <- stringr::str_squish(productgroup)
    productgroup <- stringr::str_trim(productgroup)

    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    d <- tradestatistics::ots_products %>%
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

    productname <- iconv(productname, to = "ASCII//TRANSLIT", sub = " ")
    productname <- stringr::str_replace_all(productname, "[^[:alpha:]]", " ")
    productname <- stringr::str_squish(productname)
    productname <- stringr::str_trim(productname)

    productgroup <- iconv(productgroup, to = "ASCII//TRANSLIT", sub = " ")
    productgroup <- stringr::str_replace_all(productgroup, "[^[:alpha:]]", " ")
    productgroup <- stringr::str_squish(productgroup)
    productgroup <- stringr::str_trim(productgroup)

    # get the products dataset, create the type_product column,
    # bind them all together and do the search
    d <- tradestatistics::ots_products %>%
      dplyr::mutate(
        type_name = productname,
        type_group = productgroup
      ) %>%
      dplyr::filter(
        stringr::str_detect(
          stringr::str_to_lower(!!sym("product_fullname_english")), productname
        ),
        stringr::str_detect(
          stringr::str_to_lower(!!sym("group_name")), productgroup
        )
      )
  }

  return(d)
}
