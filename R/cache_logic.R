
with_cache <- function(use_cache, file, memoised, unmemoised, 
                       read_function = read.csv, 
                       write_function = function(data, file) write.csv(data, file, row.names = FALSE), 
                       ...) {
  # cache in memory
  if (use_cache && is.null(file)) {
    return(memoised(...))
  }
  
  # cache in file  
  if (use_cache && file.exists(file)) {
    return(read_function(file))
  }
  
  if(!curl::has_internet()){
    message("A working internet connection is required to download data from tradestatistics")
    return(NULL)
  }
  data <- unmemoised(...)
  
  if (!is.null(file)) { 
    write_function(data, file)
  }
  
  if (!use_cache) {
    memoise::forget(memoised)
  } 
  
  return(data)
}


write_ots_data <- function(data, file) {
  data.table::fwrite(data, file, yaml = TRUE)
}

read_ots_data <- function(file) {
  data <- data.table::fread(file, yaml = TRUE)
  attr(data, "yaml_metadata") <- NULL
  class(data) <- c("tbl_df", "tbl", "data.frame")
  return(data)
}

