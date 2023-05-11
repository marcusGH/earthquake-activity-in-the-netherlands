library(yaml)
library(here)
library(rprojroot)

set_data_variable <- function(key, value) {
  proj_root <- find_root(has_file("README.md"))
  config <- yaml.load_file(here(proj_root, "config.yaml"))
  # read the existing data, in case key already exists
  filename <- here(proj_root, "data", "derived", config$report_summary_filename)
  data <- read.csv(filename)
  
  # key already exists
  if (any(data$key == key)) {
    data[which(data$key == key), 2] <- value
  } else {
    new_row <- data.frame(key, value)
    data <- rbind(data, new_row)
  }
  
  # save the modified data csv
  write.csv(data, filename, row.names = FALSE)
}
