#' Print a reslr output object which is created from the \code{reslr_mcmc} function.
#'
#' This will be very high level printing that the user can use to obtain information about the MCMC run using JAGS. The number of iterations and chains used by the user is printed
#'
#' @param x An object of class \code{reslr_output}
#' @param ... Other arguments (not supported)
#'
#' @return Returns high level information about the reslr_output object, i.e. the number of iterations and chains used.
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
