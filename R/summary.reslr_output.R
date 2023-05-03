#' Produces summaries and convergence diagnostics for an object created with \code{\link{reslr_mcmc}}.
#'
#' A warning message will appear if the model has not been converge. If this appears the user is recommended to re-run the model and alter the \code{reslr_mcmc} function default iteration and MCMC settings. Also, it provides high-level summaries of the estimated parameters.
#'
#' @param object Output object from the \code{\link{reslr_mcmc}}
#' @param ... Not in use
#'
#' @return A list containing convergence diagnostics and parameter estimates for the output.
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' input_data <- reslr_load(data = data)
#' jags_output <- reslr_mcmc(input_data = input_data, model_type = "eiv_slr_t")
#' summary(object = jags_output)
summary.reslr_output <- function(object, # jags_output,#
                                 # type = c("diagnostics", "parameter_estimates"),# "quantiles", "statistics",#' @param type User decides which type of summary they require
                                 ...) {
  mu_pred <- sd <- mad <- rhat <- q5 <- q95 <- alpha <- cp <- variable <- sigma_g <- phi <- sigma <- NULL
  jags_output <- object
  jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
  sample_draws <- tidybayes::tidy_draws(jags_output_model_run)

  # EIV slr t -------
  # if("parameter_estimates" %in% type){
  if (inherits(jags_output, "eiv_slr_t") == TRUE) {
    jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
    sample_draws <- tidybayes::tidy_draws(jags_output_model_run)
    # if("parameter_estimates" %in% type){
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "alpha", "beta", "sigma_res"
      )) %>%
      dplyr::select(
        variable = variable,
        mean = mean, #* mod$scale_factor_y,
        sd = sd, #* mod$scale_factor_y,
        mad = mad, #* mod$scale_factor_y, # WHAT this one?
        q5 = q5, #* mod$scale_factor_y,
        q95 = q95, # * mod$scale_factor_y
        rhat = rhat
      )
    # return(par_summary)
    # }
    # if ("diagnostics" %in% type) {
    # Check convergence
    # if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
    #   cat("No convergence issues detected. \n")
    # }
    # if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
    #   cat("Convergence issues detected. \n")
    #   cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
    # }
    # }
  }
  # EIV cp 1------
  if (inherits(jags_output, "eiv_cp1_t") == TRUE) {
    jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
    sample_draws <- tidybayes::tidy_draws(jags_output_model_run)
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "alpha", "beta[1]", "beta[2]", "cp", "sigma_res"
      )) %>%
      dplyr::select(
        variable = variable,
        mean = mean, #* mod$scale_factor_y,
        sd = sd, #* mod$scale_factor_y,
        mad = mad, #* mod$scale_factor_y, # WHAT this one?
        q5 = q5, #* mod$scale_factor_y,
        q95 = q95, # * mod$scale_factor_y
        rhat = rhat
      ) %>%
      mutate(variable = ifelse(variable == "cp", "Change Point:", variable)) %>%
      mutate(mean = ifelse(variable == "Change Point:", round(mean * 1000), mean))
  }

  # EIV cp 2
  if (inherits(jags_output, "eiv_cp2_t") == TRUE) {
    jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
    sample_draws <- tidybayes::tidy_draws(jags_output_model_run)
    # if("parameter_estimates" %in% type){
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "alpha[1]", "alpha[2]",
        "beta[1]", "beta[2]",
        "beta[3]", "cp[1]",
        "cp[2]", "sigma_res"
      )) %>%
      dplyr::select(
        variable = variable,
        mean = mean, #* mod$scale_factor_y,
        sd = sd, #* mod$scale_factor_y,
        mad = mad, #* mod$scale_factor_y, # WHAT this one?
        q5 = q5, #* mod$scale_factor_y,
        q95 = q95, # * mod$scale_factor_y
        rhat = rhat
      ) %>%
      mutate(variable = ifelse(variable == "cp[1]", "Change Point 1:", variable)) %>%
      mutate(variable = ifelse(variable == "cp[2]", "Change Point 2:", variable)) %>%
      mutate(
        mean = ifelse(variable == "Change Point 1:", round(mean * 1000), mean),
        mean = ifelse(variable == "Change Point 2:", round(mean * 1000), mean)
      )
  }

  # EIV cp 3
  if (inherits(jags_output, "eiv_cp3_t") == TRUE) {
    jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
    sample_draws <- tidybayes::tidy_draws(jags_output_model_run)
    # if("parameter_estimates" %in% type){
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "alpha[1]", "alpha[2]",
        "alpha[3]", "beta[1]", "beta[2]",
        "beta[3]", "beta[4]", "cp[1]",
        "cp[2]", "cp[3]", "sigma_res"
      )) %>%
      dplyr::select(
        variable = variable,
        mean = mean, #* mod$scale_factor_y,
        sd = sd, #* mod$scale_factor_y,
        mad = mad, #* mod$scale_factor_y, # WHAT this one?
        q5 = q5, #* mod$scale_factor_y,
        q95 = q95, # * mod$scale_factor_y
        rhat = rhat
      ) %>%
      mutate(variable = ifelse(variable == "cp[1]", "Change Point 1:", variable)) %>%
      mutate(variable = ifelse(variable == "cp[2]", "Change Point 2:", variable)) %>%
      mutate(variable = ifelse(variable == "cp[3]", "Change Point 3:", variable)) %>%
      mutate(
        mean = ifelse(variable == "Change Point 1:", round(mean * 1000), mean),
        mean = ifelse(variable == "Change Point 2:", round(mean * 1000), mean),
        mean = ifelse(variable == "Change Point 3:", round(mean * 1000), mean),
      )
  }

  # EIV IGP t
  if (inherits(jags_output, "eiv_igp_t") == TRUE) {
    jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
    sample_draws <- tidybayes::tidy_draws(jags_output_model_run)

    # if("parameter_estimates" %in% type){
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "phi", "sigma_g", "sigma_res"
      )) %>%
      dplyr::select(
        variable = variable,
        mean = mean, #* mod$scale_factor_y,
        sd = sd, #* mod$scale_factor_y,
        mad = mad, #* mod$scale_factor_y, # WHAT this one?
        q5 = q5, #* mod$scale_factor_y,
        q95 = q95, # * mod$scale_factor_y
        rhat = rhat
      )
    # par_summary
    # # }
    # # if ("diagnostics" %in% type) {
    # # Check convergence
    # if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
    #   cat("No convergence issues detected. \n")
    # }
    # if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
    #   cat("Convergence issues detected. \n")
    #   cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
    # }
    # # }
  }


  # NI spline t
  if (inherits(jags_output, "ni_spline_t") == TRUE) {
    jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
    sample_draws <- tidybayes::tidy_draws(jags_output_model_run)
    # if("parameter_estimates" %in% type){
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "b_t", "sigma_t", "sigma_res"
      )) %>%
      dplyr::select(
        variable = variable,
        mean = mean, #* mod$scale_factor_y,
        sd = sd, #* mod$scale_factor_y,
        mad = mad, #* mod$scale_factor_y, # WHAT this one?
        q5 = q5, #* mod$scale_factor_y,
        q95 = q95, # * mod$scale_factor_y
        rhat = rhat
      )
    # par_summary
    # # }
    # # if ("diagnostics" %in% type) {
    # # Check convergence
    # if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
    #   cat("No convergence issues detected. \n")
    # }
    # if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
    #   cat("Convergence issues detected. \n")
    #   cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
    # }
    # # }
  }

  # NI Spline st
  if (inherits(jags_output, "ni_spline_st") == TRUE) {
    jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
    sample_draws <- tidybayes::tidy_draws(jags_output_model_run)
    # if("parameter_estimates" %in% type){
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "b_st", "sigma_st", "sigma_res"
      )) %>%
      dplyr::select(
        variable = variable,
        mean = mean, #* mod$scale_factor_y,
        sd = sd, #* mod$scale_factor_y,
        mad = mad, #* mod$scale_factor_y, # WHAT this one?
        q5 = q5, #* mod$scale_factor_y,
        q95 = q95, # * mod$scale_factor_y
        rhat = rhat
      )
  }
  # NI GAM decomposition
  if (inherits(jags_output, "ni_gam_decomp") == TRUE) {
    jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
    sample_draws <- tidybayes::tidy_draws(jags_output_model_run)
    # if("parameter_estimates" %in% type){
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "sigma_st",
        "sigma_t[1]", "sigma_res"
      )) %>%
      dplyr::select(
        variable = variable,
        mean = mean, #* mod$scale_factor_y,
        sd = sd, #* mod$scale_factor_y,
        mad = mad, #* mod$scale_factor_y, # WHAT this one?
        q5 = q5, #* mod$scale_factor_y,
        q95 = q95, # * mod$scale_factor_y
        rhat = rhat
      )
    # par_summary
    # # }
    # # if ("diagnostics" %in% type) {
    # # Check convergence
    # if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
    #   cat("No convergence issues detected. \n")
    # }
    # if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
    #   cat("Convergence issues detected. \n")
    #   cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
    # }
    # # }
  }
  # }

  # Check convergence
  if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
    cat("No convergence issues detected. \n")
  }
  if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
    cat("Convergence issues detected. \n")
    cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
  }
  # }
  return(par_summary)
  # }
}
