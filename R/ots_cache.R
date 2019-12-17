#' Caching wrapper to reduce API calls (internal)
#' @description Eases saving the data downloaded from \code{api.tradestatistics.io}
#' and prevents \code{ots_read_from_api()} from downloading the same twice.
#' @param use_cache Logical to save and load from cache. If \code{TRUE}, the results will be cached in memory
#' if \code{file} is \code{NULL} or on disk if `file` is not \code{NULL}.
#' @param file Character with the full file path to save the data.
#' @param ... Additional parameters inherited from \code{ots_create_tidy_data()}.
#' @importFrom data.table fread fwrite
#' @importFrom digest digest
#' @importFrom memoise forget
#' @export
#' @keywords internal
ots_cache <- function(use_cache, file, ...) {
  # cache in memory ----
  if (use_cache == TRUE && is.null(file)) {
    return(ots_create_tidy_data_memoised(...))
  }
  
  # cache in file ----
  if (!is.null(file)) {
    hash <- digest::digest(list(body(ots_create_tidy_data_unmemoised), ...))
  }
  
  if (use_cache == TRUE && file.exists(file)) {
    d <- data.table::fread(file, yaml = TRUE)
    
    if (d$.hash[1] == hash) {
      class(d) <- c("tbl_df", "tbl", "data.frame")
      d$.hash <- NULL
      return(d)
    }
  }
  
  d <- ots_create_tidy_data_unmemoised(...)
  
  if (!is.null(file)) {
    d$.hash <- hash
    data.table::fwrite(d, file, yaml = TRUE)
    d$.hash <- NULL
  }
  
  if (use_cache == FALSE) {
    memoise::forget(ots_create_tidy_data_memoised)
  }
  
  return(d)
}
