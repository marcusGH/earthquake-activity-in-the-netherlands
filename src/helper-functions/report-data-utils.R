library(yaml)
library(here)
library(rprojroot)

#' Writes specified key-value pair to external data file for use
#' in latex documents through the datatool package
#'
#' The filename of the external data file is configured in the
#' config with name `report_summary_filename`
#'
#' @param key a string key to identify the value
#' @param value the value to associate with the key. Can be both
#' numeric and character type
#'
#' @examples
#' set_data_variable("pi", 3.14)
set_data_variable <- function(key, value) {
  proj_root <- find_root(has_file("README.md"))
  config <- yaml.load_file(here(proj_root, "config.yaml"))
  # read the existing data, in case key already exists
  filename <- here(proj_root, "data", "derived", config$report_summary_filename)
  
  if (file.exists(filename)) {
    data <- read.csv(filename, stringsAsFactors = FALSE)
  } else {
    data <- data.frame(key = factor(), value = character(), stringsAsFactors = FALSE)
  }
  
  # key already exists
  if (any(data$key == key)) {
    data[which(data$key == key), 2] <- value
  } else {
    new_row <- data.frame(key, value, stringsAsFactors = FALSE)
    data <- rbind(data, new_row)
  }
  
  # save the modified data csv
  write.csv(data, filename, row.names = FALSE)
}

#' Reads the value associated with specified key from the
#' external data file, configured in the config file
#'
#' An error is thrown if the key does not exist.
#'
#' @param key the string key to look up
#'
#' @return the associated value
#'
#' @examples
#' read_data_variable("pi")
read_data_variable <- function(key) {
  proj_root <- find_root(has_file("README.md"))
  config <- yaml.load_file(here(proj_root, "config.yaml"))
  # read the existing data, in case key already exists
  filename <- here(proj_root, "data", "derived", config$report_summary_filename)
  
  if (!file.exists(filename)) {
    stop("The report summary data file has not been created yet. Please call set_data_variable first.")
  }
  
  data <- read.csv(filename)
  
  # key exists
  if (any(data$key == key)) {
    return (as.character(data[which(data$key == key), 2]))
  } else {
    stop(paste0("The specifified key does not exist: ", key))
  }
}
