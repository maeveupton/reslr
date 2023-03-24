#' Summarise the output created with \code{\link{reslr_mcmc}}
#' Produces summaries and convergence diagnostics for an object created with \code{\link{reslr_mcmc}}.
#' The different options are:
#' "diagnostics" to assess MCMC convergence
#'
#' @param object Output object from the \code{\link{reslr_mcmc}}
#' @param ... Not in use
#'
#' @return A list containing convergence diagnostics
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' input_data <- reslr_load(data = data)
#' jags_output <- reslr_mcmc(input_data = input_data, model_type = "eiv_slr_t")
#' summary(object = jags_output)
summary.reslr_output <- function(object, # jags_output,
                                 # type = c("diagnostics", "parameter_estimates"),# "quantiles", "statistics",#' @param type User decides which type of summary they require
                                 ...) {
  mu_pred <- object <- jags_output <- sd <- mad <- q5 <- q95 <- alpha <- cp <- variable <- sigma_g <- phi <- sigma <- NULL
   # if(inherits(object, "reslr_output") == TRUE){
   #
   #   jags_output <- object
   #   jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
   #   sample_draws <- tidybayes::tidy_draws(jags_output_model_run)

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
      dplyr::mutate(
        par_mean = mean, #* mod$scale_factor_y,
        par_sd = sd, #* mod$scale_factor_y,
        par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
        par_q5 = q5, #* mod$scale_factor_y,
        par_q95 = q95 # * mod$scale_factor_y
      )
    par_summary
    # }
    # if ("diagnostics" %in% type) {
    # Check convergence
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
      cat("No convergence issues detected. \n")
    }
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
      cat("Convergence issues detected. \n")
      cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
    }
    # }
  }
  # EIV cp 1------
  if (inherits(jags_output, "eiv_cp1_t") == TRUE) {
    jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
    sample_draws <- tidybayes::tidy_draws(jags_output_model_run)
    # if("parameter_estimates" %in% type){
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "alpha", "beta[1]", "beta[2]", "cp", "sigma_res"
      )) %>%
      dplyr::mutate(
        par_mean = mean, #* mod$scale_factor_y,
        par_sd = sd, #* mod$scale_factor_y,
        par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
        par_q5 = q5, #* mod$scale_factor_y,
        par_q95 = q95 # * mod$scale_factor_y
      )
    par_summary
    # }
    # if ("diagnostics" %in% type) {
    #Check convergence
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
      cat("No convergence issues detected. \n")
    }
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
      cat("Convergence issues detected. \n")
      cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
    }
   # }
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
      dplyr::mutate(
        par_mean = mean, #* mod$scale_factor_y,
        par_sd = sd, #* mod$scale_factor_y,
        par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
        par_q5 = q5, #* mod$scale_factor_y,
        par_q95 = q95 # * mod$scale_factor_y
      )
    par_summary
    # }
    # if ("diagnostics" %in% type) {
    # Check convergence
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
      cat("No convergence issues detected. \n")
    }
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
      cat("Convergence issues detected. \n")
      cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
    }
    # }
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
      dplyr::mutate(
        par_mean = mean, #* mod$scale_factor_y,
        par_sd = sd, #* mod$scale_factor_y,
        par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
        par_q5 = q5, #* mod$scale_factor_y,
        par_q95 = q95 # * mod$scale_factor_y
      )
    par_summary
    # }
    # if ("diagnostics" %in% type) {
    # Check convergence
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
      cat("No convergence issues detected. \n")
    }
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
      cat("Convergence issues detected. \n")
      cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
    }
    # }
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
      dplyr::mutate(
        par_mean = mean, #* mod$scale_factor_y,
        par_sd = sd, #* mod$scale_factor_y,
        par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
        par_q5 = q5, #* mod$scale_factor_y,
        par_q95 = q95 # * mod$scale_factor_y
      )
    par_summary
    # }
    # if ("diagnostics" %in% type) {
    # Check convergence
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
      cat("No convergence issues detected. \n")
    }
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
      cat("Convergence issues detected. \n")
      cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
    }
    # }
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
      dplyr::mutate(
        par_mean = mean, #* mod$scale_factor_y,
        par_sd = sd, #* mod$scale_factor_y,
        par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
        par_q5 = q5, #* mod$scale_factor_y,
        par_q95 = q95 # * mod$scale_factor_y
      )
    par_summary
    # }
    # if ("diagnostics" %in% type) {
    # Check convergence
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
      cat("No convergence issues detected. \n")
    }
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
      cat("Convergence issues detected. \n")
      cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
    }
    # }
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
      dplyr::mutate(
        par_mean = mean, #* mod$scale_factor_y,
        par_sd = sd, #* mod$scale_factor_y,
        par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
        par_q5 = q5, #* mod$scale_factor_y,
        par_q95 = q95 # * mod$scale_factor_y
      )
    par_summary
    # }

    # if ("diagnostics" %in% type) {
    # Check convergence
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
      cat("No convergence issues detected. \n")
    }
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
      cat("Convergence issues detected. \n")
      cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
    }
    # }
  }
  # NI GAM decomposition
  if (inherits(jags_output, "ni_gam_decomp") == TRUE) {
    jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
    sample_draws <- tidybayes::tidy_draws(jags_output_model_run)
    # if("parameter_estimates" %in% type){
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "b_t", "g_h_z_x",
        "b_st", "sigma_st",
        "sigma_t", "sigma_res"
      )) %>%
      dplyr::mutate(
        par_mean = mean, #* mod$scale_factor_y,
        par_sd = sd, #* mod$scale_factor_y,
        par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
        par_q5 = q5, #* mod$scale_factor_y,
        par_q95 = q95 # * mod$scale_factor_y
      )
    par_summary
    # }
    # if ("diagnostics" %in% type) {
    # Check convergence
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
      cat("No convergence issues detected. \n")
    }
    if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
      cat("Convergence issues detected. \n")
      cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
    }
    # }
  }
  # }

  # # Check convergence
  # if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
  #   cat("No convergence issues detected. \n")
  # }
  # if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
  #   cat("Convergence issues detected. \n")
  #   cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
  # }
  # }
 # return(par_summary)
  # }
}
