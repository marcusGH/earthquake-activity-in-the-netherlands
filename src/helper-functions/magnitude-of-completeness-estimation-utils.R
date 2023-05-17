library(yaml)
library(here)
library(rprojroot)

#' Fits an exponential model to the provided magnitude data above
#' mc, and returns the fit along with a metric for its goodness of fit
#' 
#' The metric for the goodness of fit is computed through the following
#' method, first proposed by (Wiemer and Wyss) in 2000. 
#' The magnitudes are first binned based on the provided `breaks`, where
#' \eqn{N(m)} denotes the number of earthquakes in the bin with a lower value
#' of \eqn{m}. The following linear model is then fitted, only using magnitudes
#' above specified `mc`:
#' \deqn{\log N(m) = a-b\cdot m},
#' where \eqn{a} and \eqn{b} are unknown parameters. The parameters are
#' used to predict the earthquake frequencies for all the bins above mc.
#' The cumulative count for these predictions, denoted \eqn{S_i},
#' are then compared to the cumulative true counts, \eqn{B_i}, of
#' \eqn{N(m)} for \eqn{m \geq m_c}, and the sum of the absolute differences
#' are normalized by the sum of the true counts. This value then becomes
#' the residual. The goodness of fit is then 1 minus the residual, and converted
#' to percentages. All in all, we have that the goodness of fit is given by
#' \deqn{100-100\frac{ \sum_{M_i=M_j}^{M_{max}} |B_i - S_i| }{ \sum_{M_i=M_j}^{M_{max}} B_i }
#' 
#' References:
#'   Stefan Wiemer and Max Wyss. Minimum magnitude of completeness in earthquake catalogs: Examples from
#'   alaska, the western united states, and japan. Bulletin of the Seismological Society of America, 90:859–869,
#'   09 2000. doi: 10.1785/0119990114.
#' 
#'
#' @param breaks a vector of breaks to use when binning the data. Should be evenly spaced
#' @param mags a vector of magnitude values
#' @param mc the candidate mc for which to compute goodness of fit. Should be numerically close to one of the breaks
#'
#' @return a list of the R_value, measuring goodness of fit,
#' a and b, the parameters of the fitted model
goodness_of_fit <- function(breaks, mags, mc) {
  proj_root <- find_root(has_file("README.md"))
  config <- yaml.load_file(here(proj_root, "config.yaml"))
  
  # when evaluating the fit, we only make predictions on bins above mc
  # as this data is assumed complete. We assume one of the breaks
  # is equal or close to the hypothesized m_c value, or we have to fail
  mc_break_index = which(abs(breaks - mc) <= 1E-3)
  if (length(mc_break_index) != 1) {
    stop(paste("The mc parameter", mc, "must be in the list of bin breaks: ", paste(breaks, collapse = ", ")))
  }
  if (mc < min(mags) || mc >= max(mags)) {
    stop(paste("The mc parameter", mc, " is not within the range of the provided data: (", min(mags), ", ", max(mags), ")"))
  }
  
  # count the number of observations that fall into each bin
  histogram_obj <- hist(mags, breaks = breaks, plot = FALSE)
  # only consider data past and including the hypothesized m_c value
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
    )) / sum(counts_true)
  
  return (list(R_value=R_value, a=a, b=b))
}

#' Finds the optimal m_c value based on provided magnitude data
#' and configured parameters
#' 
#' Candidate values for the minimum magnitude of completeness, m_c,
#' are set to be from an evenly spaced grid from min_mc to max_mc,
#' which can be configured in config.yaml, with a spacing of the
#' the configured bin width. The goodness of fit for all of these
#' candidates are then computed using `goodness_of_fit`, and the lowest
#' m_c value achieving a goodness above the configured `explained_variance_min`
#' is used. If no such fit is found, the one giving the highest goodness of fit
#' is used instead.
#'
#' @param mags a vector of magnitude levels
#'
#' @return a list with keys:
#'         * mcs the candidate mc values considered
#'         * Rs the goodness of fits for the different candidates
#'         * as the a parameters in the exponential fits
#'         * bs the b parameters in the exponential fits
#'         * mc_hat_index the index into mcs, Rs, as and bs for which the
#'                        chosen m_c value was selected
#'         * breaks the bin-breaks used. These should also be used for plotting
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
  
  # We might get numerical errors during the binning if the breaks
  # are exactly equal to the magnitudes, so shift all the breaks
  # except the last one slightly to the left
  breaks[1:length(breaks)-1] <- breaks[1:length(breaks)-1] - 1E-4
  
  for (i in 1:length(mc_values)) {
    # find the goodness of fit
    res <- goodness_of_fit(breaks, mags, mc_values[i])
    Rs[i] <- res$R_value
    as[i] <- res$a
    bs[i] <- res$b
  }
  
  # pick the first m_c which explains above 90% of the data,
  # or whatever value is configured
  mc_hat_index <- which(Rs >= config$explained_variance_min)[1]
  if (is.na(mc_hat_index)) {
    warning(paste0("Failed to find an Mc value producing a fit that explains above ",
            config$explained_variance_min,
            "% of the data. Using the Mc value that gives the highest goodness of fit instead"))
    mc_hat_index <- which.max(Rs)
  }
  
  return (list(mcs=mc_values, Rs=Rs, as=as, bs=bs, mc_hat_index=mc_hat_index, breaks=breaks))
}