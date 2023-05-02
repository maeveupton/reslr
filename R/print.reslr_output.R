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
    if (inherits(jags_output, "eiv_slr_t") == TRUE) {
      cat("The model used was the Errors-in-Variables simple linear regression model.\n")
    }
    if (inherits(jags_output, "eiv_cp1_t") == TRUE) {
      cat("The model used was the Errors-in-Variables change point model with 1 change point.\n")
    }
    if (inherits(jags_output, "eiv_cp2_t") == TRUE) {
      cat("The model used was the Errors-in-Variables change point model with 2 change points.\n")
    }
    if (inherits(jags_output, "eiv_cp3_t") == TRUE) {
      cat("The model used was the Errors-in-Variables change point model with 3 change point.\n")
    }
    if (inherits(jags_output, "eiv_igp_t") == TRUE) {
      if(inherits(jags_output,"detrend_data")==TRUE){
        cat("The model used was the Errors-in-Variables integrated gaussian process model in time for detrended data.\n")
      }
      else{
      cat("The model used was the Errors-in-Variables integrated gaussian process model in time.\n")
      }
    }
    if (inherits(jags_output, "ni_spline_t") == TRUE) {
      cat("The model used was the Noisy Input spline in time model.\n")
    }
    if (inherits(jags_output, "ni_spline_st") == TRUE) {
      cat("The model used was the Noisy Input spline in space time model.\n")
    }
    if (inherits(jags_output, "ni_gam_decomp") == TRUE) {
      cat("The model used was the Noisy Input Generalised Additive Model for decomposition of the RSL signal.\n")
    }

    cat("The input data has been run via reslr_mcmc and has produced ")
    cat(nrow(jags_output$noisy_model_run_output$BUGSoutput$sims.matrix),
        "iterations over", jags_output$noisy_model_run_output$BUGSoutput$n.chains, "MCMC chains.")
    cat("\n\n")
  }
