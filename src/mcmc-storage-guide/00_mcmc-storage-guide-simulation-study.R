library(here)
library(rprojroot)
library(ggplot2)
library(yaml)
library(tibble)
library(dplyr)
library(tidyr)
library(magrittr)

proj_root <- find_root(has_file("README.md"))
config <- yaml.load_file(here(proj_root, "config.yaml"))

# load the helper functions required for estimating m_c
source(here(proj_root, "src", "helper-functions", "mcmc-simulation-utils.R"))
source(here(proj_root, "src", "helper-functions", "report-data-utils.R"))

p <- as.integer(config$num_model_parameters)
max_num_samples <- as.integer(config$max_num_samples)
# to get an idea of how performance scales, simulate the sampling for
# logarithmicaly spaced m-values going up to specified number of samples
ms <- as.integer(10 ^ seq(0, log10(max_num_samples), by = .25))
# we might get multiple "m=1"s, in which case increment the second one
# to avoid duplicate m-values
if (ms[2] == ms[1]) {
  ms[2] <- ms[2] + 1
}
# number of times to repeat each combination of storage allocation, m, and p
# in order to get a confidence interval for the values
num_repeats <- as.integer(config$num_repeated_simulations)

allocation_strategies <- list(
  # data frame row-wise
  list(
    storage_func = function(m, p) data.frame(matrix(NA, nrow = m, ncol = p)),
    fill_rowwise = TRUE,
    transform = function(x) x
  ),
  # data frame column-wise
  list(
    storage_func = function(m, p) data.frame(matrix(NA, nrow = p, ncol = m)),
    fill_rowwise = FALSE,
    transform = function(x) t(x)
  ),
  # matrix row-wise
  list(
    storage_func = function(m, p) matrix(NA, nrow = m, ncol = p),
    fill_rowwise = TRUE,
    transform = function(x) data.frame(x)
  ),
  # matrix column-wise
  list(
    storage_func = function(m, p) matrix(NA, nrow = p, ncol = m),
    fill_rowwise = FALSE,
    transform = function(x) data.frame(t(x))
  )
)
# entry (i, j) gives execution time for jth m-value using strategy i, where
# i  |   strategy description
# ----------------------------
# 1  | data frame row-wise
# 2  | data frame column-wise
# 3  | matrix row-wise
# 4  | matrix column-wise
time_elapsed_mean <- matrix(NA, nrow = length(allocation_strategies), ncol = length(ms))
time_elapsed_sd <- matrix(NA, nrow = length(allocation_strategies), ncol = length(ms))

# using jth m-value
for (j in 1:length(ms)) {
  # simple progress bar
  print(paste0("[", j, " / ", length(ms), "] Simulating MCMC performance for m = ", ms[j]))
  
  # using ith allocation strategy
  for (i in 1:length(allocation_strategies)) {
    times <- rep(NA, num_repeats)
    for (k in 1:num_repeats) {
      # perform the simulation
      times[k] <- simulate_mcmc_sampling(
        dummy_mcmc_sampler, allocation_strategies[[i]], ms[j], p)$time_elapsed
    }
    
    time_elapsed_mean[i, j] <- mean(times)
    time_elapsed_sd[i, j] <- sd(times)
  }
}

# plot the performance scaling
mean_data <- t(time_elapsed_mean)
sd_data <- t(time_elapsed_sd)
colnames(mean_data) <- colnames(sd_data) <-
  c("data frame row-wise", "data frame column-wise",
    "matrix row-wise", "matrix column-wise")

# pivot the data to longer to prepare for plotting
mean_data <- as_tibble(mean_data) %>%
  mutate(m = ms) %>%
  pivot_longer(
    cols = !m,
    names_to = "strategy",
    values_to = "time_avg"
  ) 
sd_data <- as_tibble(sd_data) %>%
  mutate(m = ms) %>%
  pivot_longer(
    cols = !m,
    names_to = "strategy",
    values_to = "time_sd"
  ) 
# create the 95% asymptotic normal confidence intervals
plot_data <- mean_data %>%
  inner_join(sd_data, by = c("strategy", "m")) %>%
  mutate(lower = time_avg - 1.96 * time_sd) %>%
  mutate(upper = time_avg + 1.96 * time_sd)

# produce the final plot and export to latex report
ggplot(data = plot_data, aes(x = m, y = time_avg, color = strategy)) +
  geom_point() +
  geom_line() +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    linetype = 2,
    alpha = 0.1,
  ) +
  xlab("Number of MCMC samples") +
  ylab("Execution time [seconds]") +
  guides(color=guide_legend(title="Storage allocation strategy")) +
  ggtitle("Execution time scaling of 4 different MCMC storage allocation strategies")
ggsave(here(proj_root, "outputs", "figures", "mcmc-execution-times.pdf"),
       width=18, height=9, units="cm")

# for creating a numerical summary table in the report
matrix_times <- plot_data %>%
  # the first two rows will be matrix row-wise and matrix col-wise
  # for the highest m-value
  arrange(desc(m), desc(strategy)) %>%
  head(2) %>%
  select(strategy, time_avg, time_sd)

# numerical summaries we will use in the guide supplement document
set_data_variable("max_m", ms[length(ms)])
set_data_variable("num_model_parameters", config$num_model_parameters)
set_data_variable("num_simulation_repeats", num_repeats)
set_data_variable("mat_row_avg", round(matrix_times$time_avg[1], digits = 4))
set_data_variable("mat_col_avg", round(matrix_times$time_avg[2], digits = 4))
# 95% confidence interval
set_data_variable("mat_row_95", round(1.96 * matrix_times$time_sd[1], digits = 4))
set_data_variable("mat_col_95", round(1.96 * matrix_times$time_sd[2], digits = 4))

