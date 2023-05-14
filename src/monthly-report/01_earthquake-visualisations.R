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
  filter(mag >= as.numeric(read_data_variable("mc"))) %>%
  filter(as.Date(date) >= as.Date(config$date) %m-% months(12)) %>%
  mutate(is_last_month = as.Date(date) >= as.Date(config$date) %m-% months(1)) %>%
  st_as_sf(., coords = c('lon', 'lat'), crs = "wgs84")

# fetch netherlands map geometry at high resolution on municipal level
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
  scale_y_continuous(limits = c(53.04, 53.55)) +
  scale_fill_gradient(
    low = "white",
    high = "orange",
    guide = guide_colorbar(
      title = "Total earthquake count\nin the last 12 months",
    )
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  geom_sf(data = earthquake_data, aes(
    fill = 0,
    size = mag * 4,
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
ggsave(here(proj_root, "outputs", "figures", "earthquake-map.pdf"),
       width=18, height=8.8, units="cm")

# determine how many earthquakes where in the last month compared
# to the 11 preceding months. Sorting by is_last_month makes
# the second entry be the counts for last month
group_summary <- earthquake_data %>%
  st_drop_geometry() %>%
  group_by(is_last_month) %>%
  summarise(
    n = n(),
    mag_mean = mean(mag),
    mag_max = max(mag),
    ) %>%
  arrange(by = is_last_month)

# use the same breaks for the histogram plot as what was used for binning in part 1
breaks <- seq(min(earthquake_data$mag) - config$magnitude_bin_width / 2,
              max(earthquake_data$mag) + config$magnitude_bin_width / 2, by = config$magnitude_bin_width)
ggplot(earthquake_data, aes(x=mag)) +
  geom_histogram(data = subset(earthquake_data, !is_last_month), aes(fill = is_last_month), breaks = breaks, alpha = 0.7) +
  geom_histogram(data = subset(earthquake_data, is_last_month), aes(fill = is_last_month), breaks = breaks, alpha = 0.7) +
  scale_fill_manual(
    name = "Date of occurence",
    values = c("black", "blue"),
    labels = c(paste0("Between 12 and 1 months ago (n = ", group_summary$n[1], ")"),
               paste0("Last month (n = ", group_summary$n[2], ")"))
  ) +
  scale_x_continuous(breaks = round(seq(round(min(breaks), digits=1), max(breaks), by = 0.2), 1)) +
  xlab("Magnitude") +
  ylab("Earthquake count")
ggsave(here(proj_root, "outputs", "figures", "earthquake-hist.pdf"),
       width=18, height=6, units="cm")

# numerical summaries we mention in the report

# if data is from the first in a month, the entries will mostly
# be from the previous month, so use that. Otherwise, the month
# won't change by subtracting one day
data_date <- as.Date(config$date) %m-% days(1)
set_data_variable("month_now",
                  paste(month(data_date, label = TRUE, abbr = FALSE),
                        year(data_date)))
set_data_variable("count_last_month", group_summary$n[2])
set_data_variable("count_11_month", group_summary$n[1])
set_data_variable("avg_count_11_month", round(group_summary$n[1] / 11, digits=2))
set_data_variable("max_last_month", group_summary$mag_max[2])
set_data_variable("max_11_month", group_summary$mag_max[1])
set_data_variable("mean_last_month", round(group_summary$mag_mean[2], digits=2))
set_data_variable("mean_11_month", round(group_summary$mag_mean[1], digits=2))
