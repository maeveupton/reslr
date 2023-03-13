#' Checking the algorithm converged
#'
#' @param jags_output Output from the JAGS
#'
#' @return Statement of convergence
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' input_data <- load_input_data(data = data)
#' check_convergence(jags_output = run_mcmc(input_data = input_data, model_type = "eiv_slr_t"))
check_convergence <- function(jags_output) {
  # Create an object containing the posterior samples
  m <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
  sample_draws <- tidybayes::tidy_draws(m)
  par_summary <- posterior::summarise_draws(sample_draws)
  # Check convergence
  if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) message("No convergence issues detected")
  if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) message("Convergence issues detected")
}
