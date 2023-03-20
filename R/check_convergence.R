#' Checking the algorithm converged
#'
#' @param object Output object from the \code{\link{reslr_mcmc}}
#'
#' @return Statement of convergence
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' input_data <- reslr_load(data = data)
#' jags_output <- reslr_mcmc(input_data = input_data, model_type = "eiv_slr_t")
#' check_convergence(object = jags_output)
check_convergence <- function(object) {
  # Create an object containing the posterior samples
  jags_output <- object
  m <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
  sample_draws <- tidybayes::tidy_draws(m)
  par_summary <- posterior::summarise_draws(sample_draws)
  # Check convergence
  if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) message("No convergence issues detected")
  if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) message("Convergence issues detected")
}
