library(here)
library(rprojroot)
library(tibble)
library(tidyverse)
library(ggplot2)
library(yaml)
library(dplyr)
library(magrittr)
library(lubridate)

proj_root <- find_root(has_file("README.md"))
config <- yaml.load_file(here(proj_root, "config.yaml"))

# load the helper functions required for estimating M_c
source(here(proj_root, "src", "helper-functions", "magnitude-of-completeness-estimation-utils.R"))
source(here(proj_root, "src", "helper-functions", "report-data-utils.R"))

# read in the newest data as a tibble
data_filename <- paste0(config$date, config$data_suffix, ".csv")
data_path <- here(proj_root, "data", "raw", data_filename)
if (!file.exists(data_path)) {
  stop(paste0("The earthquake data file was not found: ", data_path,
              ". Make sure the data in config.yaml is configured correctly"))
}

# only use the last 12 months
earthquake_data <- read_csv(data_path) %>%
  filter(as.Date(date) >= as.Date(config$date) %m-% months(12))

mc_estimate <- estimate_mc(earthquake_data$mag)
mc_hat <- mc_estimate$mcs[mc_estimate$mc_hat_index]
a_hat <- mc_estimate$as[mc_estimate$mc_hat_index]
b_hat <- mc_estimate$bs[mc_estimate$mc_hat_index]

ggplot(data.frame(mcs = mc_estimate$mcs, Rs = mc_estimate$Rs), aes(mcs, 100-Rs)) +
  geom_line() +
  geom_point(shape=17, size=3) +
  geom_vline(xintercept=mc_hat, linetype="dashed", 
            color = "red", linewidth=1) +
  scale_y_continuous("Residual in %", sec.axis = sec_axis(~ (100 - .), name = "Goodness of fit"), limits = c(0, NA)) +
  xlab(expression("Minimum magnitude of completeness, m"["c"])) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_blank(),
        panel.background = element_blank())
ggsave(here(proj_root, "outputs", "figures", "goodness-of-fit.pdf"),
       width=9, height=27/5, units="cm")

# in case the magnitude is equal to mc, accept some noise to avoid numerical errors
mags_above_mc <- earthquake_data$mag[which(earthquake_data$mag >= mc_hat - 1E-4)]
mag_breaks <- mc_estimate$breaks[(mc_estimate$mc_hat_index+1):length(mc_estimate$breaks)]

# pdf overlay data
xs <- seq(mc_hat, max(mags_above_mc), by=0.01)
ys <- exp(a_hat + b_hat * xs)

ggplot(data.frame(mags_above_mc), aes(mags_above_mc)) +
  xlab("Magnitude") +
  ylab("Frequency") +
  geom_histogram(breaks = mag_breaks, color = NA, fill = "black", alpha = 0.7) +
  scale_x_continuous(breaks = round(seq(min(mag_breaks), max(mag_breaks), by = 0.4), 1)) +
  geom_line(data = data.frame(xs, ys), aes(xs, ys), color = "red", linewidth = 1) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(here(proj_root, "outputs", "figures", "earthquake-histogram.pdf"),
       width=9, height=27/5, units="cm")
        
# variable data we use in the monthly report. Save these to external
# file so that they can be read by LaTeX using the datatool package
set_data_variable("mc", mc_hat)
set_data_variable("a_hat", round(a_hat, 4))
set_data_variable("b_hat", round(b_hat, 4))
set_data_variable("num_complete_observations", length(mags_above_mc))
set_data_variable("explained_variance", config$explained_variance_min)
set_data_variable("bin_width", config$magnitude_bin_width)
sf <- stamp("January 01, 1970")
set_data_variable("date_start", sf(as.Date(config$date) %m-% months(12)))
set_data_variable("date_now", sf(as.Date(config$date)))
