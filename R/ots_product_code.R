#' Obtain a table of official product names and Harmonized System (HS) codes
#' according to the United Nations nomenclature
#' @description This function takes a text string and searches within the
#' package data for all matching product codes in the context of valid API
#' product codes.
#' @param productname A text string such as "Animals", "COPPER" or "fruits".
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
#' ots_product_code("animals")
#' ots_product_code("fruits")
#' @keywords functions

ots_product_code <- function(productname = NULL) {
  stopifnot(is.character(productname))
  stopifnot(nchar(productname) > 0)
  
  productname <- iconv(productname, to = "ASCII//TRANSLIT", sub = "")
  productname <- str_replace_all(productname, "[^[:alpha:]|[:space:]]", "")
  
  # get the products dataset, create the type_product column,
  # bind them all together and do the search
  tradestatistics::ots_attributes_products %>%
    mutate(
      type_product = productname
    ) %>%
    filter(
      str_detect(
        str_to_lower(!!sym("commodity_fullname_english")), productname
      )
    )
}
