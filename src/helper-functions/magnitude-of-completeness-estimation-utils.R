library(yaml)
library(here)
library(rprojroot)

goodness_of_fit <- function(breaks, mags, mc) {
  proj_root <- find_root(has_file("README.md"))
  config <- yaml.load_file(here(proj_root, "config.yaml"))
  
  # when evaluating the fit, we only make predictions on bins above mc
  # as this data is assumed complete. We assume one of the breaks
  # is equal to the hypothesized M_c value, or we have to fail
  mc_break_index = which(abs(breaks - mc) <= 1E-5)
  if (length(mc_break_index) != 1) {
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
    logN = log(bin_counts + config$log_frequency_smoothing_parameter),
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
  
  # The goodness of fit residual, R(a, b, M_i) in (Wiemer, S., Wyss, M., 2000) 
  R_value <- 100 - 100 * sum(abs(
      counts_true - counts_pred
    )) / sum(counts_true) #sum(histogram_obj$counts)
  
  return (list(R_value=R_value, a=a, b=b))
}

estimate_mc <- function(mags) {
  proj_root <- find_root(has_file("README.md"))
  config <- yaml.load_file(here(proj_root, "config.yaml"))
  
  # make the lower limit a multiple of the bin_width such that the
  # bins are spaced in multiples of the bin_width, with one break at 0.0
  # otherwise we might get errors if min(mags) and max(mags)
  # are not both multiples of bin_width
  breaks <- seq(floor(min(mags) / config$magnitude_bin_width) * config$magnitude_bin_width,
                max(mags) + config$magnitude_bin_width, by = config$magnitude_bin_width)
  
  # try the mc values aligned with the bin breaks, skipping the first
  # candidate because we assume data is incomplete and skipping last
  # due to it causing no data to be available for estimating a and b
  # also don't use configured values if fall outside of data
  mc_values <- seq(max(config$min_mc, breaks[2]),
                   min(config$max_mc, breaks[length(breaks)-1]),
                   by = config$magnitude_bin_width)
  
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
  
  # pick the first Mc which explains above 90% of the data,
  # or whatever value is configured
  mc_hat_index <- which(Rs >= config$explained_variance_min)[1]
  if (is.na(mc_hat_index)) {
    warning(paste0("Failed to find an Mc value producing a fit that explains above ",
            config$explained_variance_min,
            "% of the data. Using the Mc value that gives the highest goodness of fit instead"))
    mc_hat_index <- which.max(Rs)
  }
  
  return (list(mcs=mc_values, Rs=Rs, as=as, bs=bs, mc_hat_index=mc_hat_index))
}