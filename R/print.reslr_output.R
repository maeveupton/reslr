#' Print a reslr output object
#'
#' @param x An object of class \code{reslr_output}
#' @param ... Other arguments (not supported)
#'
#' @return Returns a neat summary of the object
#'
#' @export
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' input_data <- reslr_load(data = data)
#' jags_output <- reslr_mcmc(input_data = input_data, model_type = "eiv_slr_t")
#' print(x = jags_output)
print.reslr_output <-
  function(x, ...) {
    jags_output <- x
    cat("The input data has been run via reslr_mcmc and has produced ")
    cat(nrow(jags_output$noisy_model_run_output$BUGSoutput$sims.matrix),
        "iterations over", jags_output$noisy_model_run_output$BUGSoutput$n.chains, "MCMC chains.")
    cat("\n\n")
  }
