library(here)
library(rprojroot)
library(tibble)
library(tidyverse)
library(ggplot2)

date_str <- "2023-02-01"

root_dir <- find_root(has_file("README.md"))
tib <- read_csv(here(root_dir, "data", "raw",
                   paste0(date_str, "_induced-earthquakes.csv")))

hist(tib$mag, breaks = seq(-0.3, 4.0, by = 0.2))

#' Returns the MLE estimate beta_hat of the exponential
#' distribution, given data Xs in log scale
#'
#' @param log_xs data in log scale
#'
#' @return 
#' @export
#'
#' @examples
find_beta_hat <- function(log_xs, mc, bin_width) {
  # Richter's magnitude scale is base10
  # return (length(log_xs) / sum(log_xs))
  return (log10(exp(1)) / (mean(log_xs) - (mc - bin_width / 2)))
}

goodness_of_fit <- function(breaks, mags, mc) {
  # when evaluating the fit, we only make predictions on bins above mc
  # as this data is assumed complete. We assume one of the breaks
  # is equal to the hypothesized M_c value, or we have to fail
  mc_break_index = which(abs(breaks - mc) <= 1E-5)
  if (length(mc_break_index) != 1) {
    # TODO: test this
    stop(paste("The mc parameter", mc, "must be in the list of bin breaks: ", paste(breaks, collapse = ", ")))
  }
  if (mc < min(mags) || mc >= max(mags)) {
    stop(paste("The mc parameter", mc, " is not within the range of the provided data: (", min(mags), ", ", max(mags), ")"))
  }
  
  # count the number of observations that fall into each bin
  histogram_obj <- hist(mags, breaks = breaks, plot = FALSE)
  # only consider data past and including the hypothesized M_c value
  bin_counts <- histogram_obj$counts[mc_break_index:(length(breaks) - 1)]
  
  # Assuming mag ~ Exp(beta) gives log(N(m))=a+b*m, so fit linear model
  # to decide a and b, where m is the magnitude, and N(m) is the number
  # of events in the corresponding magnitude bin
  mod <- lm(logN ~ 1 + m, data = data.frame(
    # add one smoothing to avoid -Inf values
    logN = log(bin_counts + 0.1),
    # use the lower limit as the predictor
    m = breaks[mc_break_index:(length(breaks) - 1)]
  ))
  a <- mod$coefficients[[1]]
  b <- mod$coefficients[[2]]
  
  # the true cumulative bin counts, B_i in (Wiemer, S., Wyss, M., 2000)
  counts_true <- cumsum(bin_counts)
  # the predicted cumulative counts in each bin, lower limit used because
  # we also used the lower bin limit when fitting the linear model above
  # (Note: This is S_i in (Wiemer, S., Wyss, M., 2000))
  counts_pred <- cumsum(exp(predict(mod)))
  
  # print("Temp")
  # print(counts_true)
  # print(counts_pred)
  # 
  # plot(mc_bin_index:length(bin_counts), counts_true[mc_bin_index:length(bin_counts)], col='red', type='l')
  # lines(mc_bin_index:length(bin_counts), counts_pred[mc_bin_index:length(bin_counts)], col='blue')
  
  # The goodness of fit residual, R(a, b, M_i) in (Wiemer, S., Wyss, M., 2000) 
  R_value <- 100 - 100 * sum(abs(
      counts_true[mc_bin_index:length(bin_counts)] - #counts_true[1:(mc_bin_index-1)] -
      counts_pred[mc_bin_index:length(bin_counts)]
    )) / sum(counts_true)
  # print(R_value)
  
  return (list(R_value=R_value, a=a, b=b))
}

# TODO: docs
estimate_mc <- function(mags, bin_width = 0.1) {
  # make the lower limit a multiple of the bin_width such that the
  # bins are spaced in multiples of the bin_width, with one break at 0.0
  # otherwise we might get errors if min(mags) and max(mags)
  # are not both multiples of bin_width
  breaks <- seq(floor(min(mags) / bin_width) * bin_width, max(mags), by = bin_width)
  
  # try the mc values aligned with the bin breaks, skipping the first
  # candidate because we assume data is incomplete and skipping last
  # due to it causing no data to be available for estimating a and b
  mc_values <- breaks[2:(length(breaks)-1)]
  # TODO: read from config Max mc value and override candidates if lower than max, and set it to 2.1 or something
  
  # store goodness of fits, along with the a and b estimates
  Rs <- rep(0, length(mc_values))
  as <- rep(0, length(mc_values))
  bs <- rep(0, length(mc_values))
  
  for (i in 1:length(mc_values)) {
    # find the goodness of fit
    res <- goodness_of_fit(breaks, mags, mc_values[i])
    Rs[i] <- res$R_value
    as[i] <- res$a
    bs[i] <- res$b
  }
  
  # TODO: config where can configure everything
  # warning("The optimal M_c value does not give a model that can explain more than TODO% of the data variability. Using Mc=TODO, which only explains TODO% of the variability")
  
  return (tibble(mcs=mc_values, Rs=Rs, as=as, bs=bs))
}

res <-estimate_mc(tib$mag, bin_width = 0.1)
best_mc <- res$mcs[which.max(res$Rs)]

# TODO: make plot better by adding x label "Magnitude" etc. and legend
ggplot(res, aes(mcs, 100-Rs)) +
  geom_line() +
  geom_point(shape=17, size=5) +
  geom_vline(xintercept=best_mc, linetype="dashed", 
            color = "red", linewidth=1) +
  scale_y_continuous("Residual in %", sec.axis = sec_axis(~ (100 - .), name = "Goodness of fit"))

# (100 - goodness) is the R-values, the sum thing, in the paper plot figure 2, left axis is residuals % (i.e. what's left)
# right axis, but flipped because goodness = 100 - R-values is good, so actually take minimum in the plot!

beta_hat <- res$betas[which.max(res$Rs)]

line_scaling <- length(tib$mag[which(tib$mag>=best_mc)])

ggplot(tib, aes(mag)) +
  geom_histogram(binwidth=.1) +
  geom_line(data=data.frame(xs=seq(best_mc, max(tib$mag), by=0.001)), aes(xs, line_scaling * dexp(xs, rate = beta_hat))) +
  geom_vline(xintercept=best_mc, linetype="dashed",  color = "red", linewidth=1)

