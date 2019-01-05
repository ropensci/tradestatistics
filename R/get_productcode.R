#' Obtain a valid product codes
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
#' @importFrom stringr str_detect str_to_lower
#' @importFrom utils data
#' @export
#' @examples
#' get_productcode("animals")
#' get_productcode("fruits")
#' @keywords functions

get_productcode <- function(productname = NULL) {
  # get the products dataset, create the type_product column, 
  # bind them all together and do the search
  tradestatistics::products %>%
    mutate(
      type_product = productname
    ) %>% 
    filter(
      str_detect(
        str_to_lower(!!sym("product_fullname_english")), productname
      )
    )
}