library(yaml)
library(here)
library(rprojroot)

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
