library(here)
library(rprojroot)
library(tibble)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(magrittr)
library(yaml)
library(lubridate)
library(eurostat)
library(scales)
library(sf)

proj_root <- find_root(has_file("README.md"))
config <- yaml.load_file(here(proj_root, "config.yaml"))

# load the helper functions required for estimating M_c
source(here(proj_root, "src", "helper-functions", "magnitude-of-completeness-estimation-utils.R"))
source(here(proj_root, "src", "helper-functions", "report-data-util.R"))

# read in the newest data as a tibble
data_filename <- paste0(config$date, config$data_suffix, ".csv")
# filter out earthquakes with a magnitude below estimated
# M_c so that we have a complete dataset, and only consider
# the past 12 months
earthquake_data <- read_csv(here(proj_root, "data", "raw", data_filename)) %>%
  filter(mag >= read_data_variable("mc")) %>%
  filter(as.Date(date) >= as.Date(config$date) %m-% months(12)) %>%
  mutate(is_last_month = as.Date(date) >= as.Date(config$date) %m-% months(1)) %>%
  st_as_sf(., coords = c('lon', 'lat'), crs = "wgs84")

# Earthquake counts
# * Easy for just numerical value for this
# * Compare these numerical values in report without plots

# Magnitudes
# * Just some simple histograms?

# Earthquake locations
# * Map of netherlands using longitude and latitude, with red dots for last month
#   and blue dots for 11 months prior or something
ggplot(earthquake_data, aes(lat, lon)) +
  geom_point()

nl_geo_data <- get_eurostat_geospatial(output_class = "sf",
                                       resolution = "1",
                                       nuts_level = 3,
                                       year = 2021) %>%
  filter(grepl("NL", CNTR_CODE))

# bin the earthquakes by the municipals to get counts
st_join(nl_geo_data, earthquake_data) %>%
  group_by(id) %>%
  summarise(earthquake_count = sum(!is.na(mag))) %>%
  ggplot(aes(fill = earthquake_count)) +
  geom_sf() +
  geom_sf_label(aes(label = earthquake_count), nudge_x = 0.07) +
  scale_x_continuous(limits = c(6.2, 7.2)) +
  scale_y_continuous(limits = c(53, 53.55)) +
  scale_fill_gradient(
    low = "white",
    high = "orange",
    guide = guide_colorbar(
      title = "Total earthquake count\nin the last 12 months",
    )
    # guide_legend(
    #   override.aes = list(shape = NA, label = ""),
    #   direction = "vertical",
    #   title.position = "top",
    #   title = "Total earthquake count\nin the last 12 months"
    #   )
  ) +
  geom_sf(data = earthquake_data, aes(
    fill = 0,
    size = mag * 10,
    alpha = ifelse(is_last_month, 0.7, 0.3),
    shape = ifelse(is_last_month, 10, 20),
    col = ifelse(is_last_month, "Last month", "Between 12 and 1 months ago")),
  ) +
  scale_color_manual(
    values = c("black", "blue"),
    guide = guide_legend(
      override.aes = list(size = 7, shape = c(20, 10)),
      title = "Date of occurence",
  )) +
  scale_shape_identity() +
  scale_size_identity() +
  scale_alpha_identity()
