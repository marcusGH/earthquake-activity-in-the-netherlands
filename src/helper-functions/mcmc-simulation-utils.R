library(tictoc)

dummy_mcmc_sampler <- function(prev_state, p) {
  # ignore state
  next_state <- prev_state
  # assume the posterior is some predefined multivariate normal
  # with identity covariance matrix
  sample <- rnorm(p)
  
  return (list(state = next_state, sample = sample))
}

# ellipsis arguments are passed on to the sampler, e.g. used for data
#'
#' @param mcmc_sampler A function taking two arguments, some
#' arbitrary state, and an integer p specifying the dimension of
#' the sample. The function should then return a list where key
#' $sample maps to a p-length numeric vector and key $state maps
#' to some arbitrary object that the will be fed back to the sampler
#' the next time it is called. The first time the sampler is called,
#' this state will be NULL. The ellipsis arguments are also passed
#' on to the mcmc_sampler function. In summary, the sampler should be able to
#' comply with the following sampling procedure:
#' 
#' state <- NULL
#' for (i in 1:m) {
#'   res <- mcmc_sampler(state, p, ...)
#'   state <- res$state
#'   sample <- res$sample
#' }
#' 
#' @param storage either a m x p or p x m sized object that numeric
#' values can be assigned to. The parameters m and p will be infered
#' from object storage and from whether it is filled row-wise or
#' column-wise. Values of dimension p are added 1-by-1 m times during
#' simulation
#' @param fill_rowwise boolean indicating whether the storage
#' should be filled row-by-row or column-by-column
#' @param ... optional arguments passed along to the mcmc sampler
#'
#' @return a list with time_elapsed and samples objects
#' @export
#'
#' @examples
simulate_mcmc_sampling <- function(mcmc_sampler, allocation_strategy, m, p, ...) {
  
  prev_mcmc_state <- NULL
  
  tic(quiet = TRUE)
  
  storage <- allocation_strategy$storage_func(m, p)
  for (i in 1:m) {
    res <- mcmc_sampler(prev_mcmc_state, p, ...)
    prev_mcmc_state <- res$state

    if (allocation_strategy$fill_rowwise) {
      storage[i,] <- res$sample
    } else {
      storage[,i] <- res$sample
    }
  }
  # transpose the whole thing if filling column-wise
  # and convert to dataframe if using matrices
  storage <- allocation_strategy$transform(storage)
  
  timer <- toc(quiet = TRUE)
  time_elapsed <- timer$toc - timer$tic
  
  return (list(time_elapsed = time_elapsed, samples = storage))
}
