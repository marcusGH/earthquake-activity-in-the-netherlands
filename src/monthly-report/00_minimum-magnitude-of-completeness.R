library(here)
library(rprojroot)
library(tibble)
library(tidyverse)
library(ggplot2)
library(yaml)

proj_root <- find_root(has_file("README.md"))
config <- yaml.load_file(here(proj_root, "config.yaml"))

# load the helper functions required for estimating M_c
source(here(proj_root, "src", "helper-functions", "magnitude-of-completeness-estimation-utils.R"))
source(here(proj_root, "src", "helper-functions", "report-data-util.R"))

# read in the newest data as a tibble
data_filename <- paste0(config$date, config$data_suffix, ".csv")
earthquake_data <- read_csv(here(proj_root, "data", "raw", data_filename))

mc_estimate <- estimate_mc(earthquake_data$mag)
best_mc <- mc_estimate$mcs[mc_estimate$best_mc_index]
best_a <- mc_estimate$as[mc_estimate$best_mc_index]
best_b <- mc_estimate$bs[mc_estimate$best_mc_index]

ggplot(data.frame(mc_estimate), aes(mcs, 100-Rs)) +
  geom_line() +
  geom_point(shape=17, size=3) +
  geom_vline(xintercept=best_mc, linetype="dashed", 
            color = "red", linewidth=1) +
  scale_y_continuous("Residual in %", sec.axis = sec_axis(~ (100 - .), name = "Goodness of fit"), limits = c(0, NA)) +
  xlab(expression("Minimum magnitude of completeness, M"["c"])) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_blank(),
        panel.background = element_blank())
ggsave(here(proj_root, "outputs", "figures", "goodness-of-fit.pdf"),
       width=9, height=27/3, units="cm")

# TODO: now make the below plot verifying that records appear complete
# After doing this, start wriiting page one of the report, citing papers etc.
# and also exporting all the config to file so can be read in by latex, e.g. M_c etc.

# histogram data
mags_above_mc <- earthquake_data$mag[which(earthquake_data$mag >= best_mc)]
mag_breaks <- seq(best_mc, max(mags_above_mc), by = config$magnitude_bin_width)
# pdf overlay data
xs <- seq(best_mc, max(mags_above_mc), by=0.01)
ys <- exp(best_a + best_b * xs)

# TODO: add legends ...
ggplot(data.frame(mags_above_mc), aes(mags_above_mc)) +
  # labs(
  #   title = expression("Earthquake frequency for magnitudes above M"["c"]),
  # ) +
  xlab("Magnitude") +
  ylab("Frequency") +
  geom_histogram(breaks = mag_breaks, color = NA, fill = "black", alpha = 0.7) +
  scale_x_continuous(breaks = round(seq(min(mag_breaks), max(mag_breaks), by = 0.4), 1)) +
  # scale_color_manual(name = "group", values = c("black" = "black", "red" = "red"), labels = c("black" = "t1", "red" = "t2")) +
  # scale_fill_manual(name = "group", values = c("black" = "black", "red" = "red"), labels = c("black" = "t1", "red" = "t2")) +
  geom_line(data = data.frame(xs, ys), aes(xs, ys), color = "red", linewidth = 1) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave(here(proj_root, "outputs", "figures", "earthquake-histogram.pdf"),
       width=9, height=27/3, units="cm")
        
set_data_variable("mc", best_mc)