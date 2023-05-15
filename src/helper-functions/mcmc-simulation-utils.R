library(tictoc)

#' Title
#'
#' @param prev_state 
#' @param p 
#'
#' @return
#' @export
#'
#' @examples
dummy_mcmc_sampler <- function(prev_state, p) {
  # ignore state
  next_state <- prev_state
  # assume the posterior is some predefined multivariate normal
  # with identity covariance matrix
  sample <- rnorm(p)
  
  return (list(state = next_state, sample = sample))
}

#' Measures the execution time required to run the provided MCMC
#' sampler to produce `m` samples of length `p`, where the storage
#' is pre-allocated using the provided `allocation_strategy`
#'
#' @param mcmc_sampler A function taking two arguments, some
#' arbitrary state, and an integer p specifying the dimension of
#' the sample. The function should then return a list where key
#' `sample` maps to a p-length numeric vector and key `state` maps
#' to some arbitrary object that the will be fed back to the sampler
#' the next time it is called. If for example using a Metropolis-Hastings
#' sampler, this state could be the previous sample. The first time
#' the sampler is called, this state will be NULL. The ellipsis
#' arguments are also passed on to the mcmc_sampler function.
#' In summary, the sampler should be able to
#' comply with the following sampling procedure:
#' 
#' state <- NULL
#' for (i in 1:m) {
#'   res <- mcmc_sampler(state, p, ...)
#'   state <- res$state
#'   sample <- res$sample
#' }
#' 
#' @param allocation_strategy a list containing the following three items:
#' * `fill_rowwise` a boolean value indicating how the storage is filled
#' * `storage_func` a function taking two integers, m, and p, and returning
#'   some storage object of size \eqn{m \times p} if `fill_rowwise` is true,
#'   and of size \eqn{p \times m} otherwise
#' * `transform` a function that takes a single argument, and will be applied to
#'   the storage object after it has been filled with samples. It should returned
#'   a transformed version of the provided storage object, for instance transposed
#' @param m the number of samples to generate
#' @param p the dimension of the samples
#' @param ... optional arguments passed along to the `mcmc_sampler` when it is called
#'
#' @return a list with time_elapsed and transformed and filled samples storage object
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
  # and convert to data frame if using matrices
  storage <- allocation_strategy$transform(storage)
  
  timer <- toc(quiet = TRUE)
  time_elapsed <- timer$toc - timer$tic
  
  return (list(time_elapsed = time_elapsed, samples = storage))
}
