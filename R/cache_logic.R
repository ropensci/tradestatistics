#' Caching wrapper to reduce API calls (internal function)
#' @description This function eases saving the data downloaded from \code{api.tradestatistics.io}
#' and prevents \code{ots_read_from_api()} from downloading the same twice.
#' @param use_cache Logical to save and load from cache. If \code{TRUE}, the results will be cached in memory
#' if \code{file} is \code{NULL} or on disk if `file` is not \code{NULL}.
#' @param memoised a memorised function (e.g. \code{ots_create_tidy_data_memoised()}).
#' @param unmemoised a memorised function (e.g. \code{ots_create_tidy_data_unmemoised()}).
#' @param file Character with the full file path to save the data.
#' @param ... additional parameters inherited from \code{ots_create_tidy_data()}.
#' @importFrom data.table fread fwrite
#' @importFrom digest digest
#' @importFrom memoise forget
#' @keywords internal

ots_with_cache <- function(use_cache, file, memoised, unmemoised, ...) {
  # cache in memory ----
  if (use_cache == TRUE & is.null(file)) {
    return(memoised(...))
  }

  if (!is.null(file)) {
    hash <- digest::digest(list(body(unmemoised), ...))
  }

  # cache in file ----
  if (use_cache == TRUE & file.exists(file)) {
    data <- data.table::fread(file, yaml = TRUE)

    if (data$.hash[1] == hash) {
      class(data) <- c("tbl_df", "tbl", "data.frame")
      data$.hash <- NULL
      return(data)
    }
  }

  data <- unmemoised(...)

  if (!is.null(file)) {
    data$.hash <- hash
    data.table::fwrite(data, file, yaml = TRUE)
    data$.hash <- NULL
  }

  if (use_cache == FALSE) {
    memoise::forget(memoised)
  }

  return(data)
}
