
with_cache <- function(use_cache, file, memoised, unmemoised,
                       ...) {
  # cache in memory
  if (use_cache && is.null(file)) {
    return(memoised(...))
  }
  
  if (!is.null(file)) { 
    hash <- digest::digest(list(body(unmemoised), ...))
  } 
  
  # cache in file  
  if (use_cache && file.exists(file)) {
    data <- data.table::fread(file, yaml = TRUE)
    
    if (data$.hash[1] == hash) {
      class(data) <- c("tbl_df", "tbl", "data.frame")
      data$.hash <- NULL
      return(data)
    }
  }
  # 
  # if(!curl::has_internet()){
  #   message("A working internet connection is required to download data from tradestatistics")
  #   return(NULL)
  # }
  data <- unmemoised(...)
  
  if (!is.null(file)) {
    data$.hash <- hash
    data.table::fwrite(data, file, yaml = TRUE)
    data$.hash <- NULL
  }
  
  if (!use_cache) {
    memoise::forget(memoised)
  } 
  
  return(data)
}

