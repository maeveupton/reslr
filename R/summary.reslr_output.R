#' Summarise the output created with \code{\link{reslr_mcmc}}
#' Produces summaries and convergence diagnostics for an object created with \code{\link{reslr_mcmc}}.
#' The different options are:
#' "diagnostics" to assess MCMC convergence
#'
#' @param object Output object from the \code{\link{reslr_mcmc}}
#' @param type User decides which type of summary they require
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
summary <- function(object,
                    type = c("diagnostics", "quantiles", "statistics", "parameter_estimate"),
                    ...) {
  UseMethod("summary.reslr_output")
}
summary.reslr_output <- function(object,
                                 type = c("diagnostics", "quantiles", "statistics", "parameter_estimate"),
                                 ...) {
  mu_pred <- .lower <- .upper <- object <- jags_output <- variable <- sd <- mad <- q5 <- q95 <- `w.m[1]` <- `w.m[50]` <- lwr_95 <- upr_95 <- alpha <- cp <- sigma_g <- phi <- sigma <- NULL
  # jags_output <- object
  #if (inherits(jags_output, "reslr_output")) {
    jags_output_model_run <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
    # Create an object containing the posterior samples
    sample_draws <- tidybayes::tidy_draws(jags_output_model_run)
    par_summary <- posterior::summarise_draws(sample_draws)

    if ("diagnostics" %in% type) {
      # Check convergence
      if (sum(par_summary$rhat > 1.1, na.rm = TRUE) == 0) {
        cat("No convergence issues detected. \n")
      }
      if (sum(par_summary$rhat > 1.1, na.rm = TRUE) > 0) {
        cat("Convergence issues detected. \n")
        cat("Increase the number of iterations to make a longer model run in reslr_mcmc \n")
      }
    }
    if ("quantiles" %in% type) {
      out_quantiles <- t(apply(jags_output_model_run, 2, "quantile",
        probs = c(0.025, 0.25, 0.5, 0.75, 0.975)
      ))
      print(round(out_quantiles, 3))
    }
    if ("statistics" %in% type) {
      out_statistics <- t(apply(jags_output_model_run, 2, function(x) {
        return(c(mean = mean(x), sd = stats::sd(x)))
      }))
      print(round(out_statistics, 3))
    }
    if ("parameter_estimate" %in% type) {
      # Estimated parameter summaries

      # EIV slr t
      if (inherits(jags_output, "eiv_slr_t") == TRUE) {
        par_summary <- posterior::summarise_draws(sample_draws) %>%
          dplyr::filter(variable %in% c(
            "alpha", "beta" # , "sigma_res"
          )) %>%
          dplyr::mutate(
            par_mean = mean, #* mod$scale_factor_y,
            par_sd = sd, #* mod$scale_factor_y,
            par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
            par_q5 = q5, #* mod$scale_factor_y,
            par_q95 = q95 # * mod$scale_factor_y
          )
        par_summary
      }

      # EIV cp 1
      if (inherits(jags_output, "eiv_cp1_t") == TRUE) {
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
      }

      # EIV cp 2
      if (inherits(jags_output, "eiv_cp2_t") == TRUE) {
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
      }

      # EIV cp 3
      if (inherits(jags_output, "eiv_cp3_t") == TRUE) {
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
      }

      # EIV IGP t
      if (inherits(jags_output, "eiv_igp_t") == TRUE) {
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
      }

      # NI spline t
      if (inherits(jags_output, "ni_spline_t") == TRUE) {
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
      }

      # NI Spline st
      if (inherits(jags_output, "ni_spline_st") == TRUE) {
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
        print(par_summary)
      }
      # NI GAM decomposition
      if (inherits(jags_output, "ni_gam_decomp") == TRUE) {
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
        print(par_summary)
      }
    }
  #}
}
