#' Summarise the output created with \code{\link{reslr_mcmc}}
#' Produces summaries and convergence diagnostics for an object created with \code{\link{reslr_mcmc}}.
#' The different options are:
#' "diagnostics" to assess MCMC convergence
#'
#' @param jags_output Output object from the \code{\link{reslr_mcmc}}
#' @param type User decides which type of summary they require
#' @param ... Not in use
#'
#' @return A list containing convergence diagnostics
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' input_data <- reslr_load(data = data)
#' jags_output  <- reslr_mcmc(input_data = input_data, model_type = "eiv_slr_t")
#' check_convergence(jags_output = jags_output, type = "diagnostics")
summary.reslr_output <- function(jags_output,
                                 type = c("diagnostics"),
                                 ...){
  jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput
  if ("diagnositcs" %in% type){
    # Create an object containing the posterior samples
    m <- jags_output_model_run$sims.matrix
    sample_draws <- tidybayes::tidy_draws(m)
    par_summary <- posterior::summarise_draws(sample_draws)
    # Check convergence
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) message("No convergence issues detected")
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) message("Convergence issues detected")

  }

}
