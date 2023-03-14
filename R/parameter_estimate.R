#' Examining the parameter estimates from JAGS
#'
#' @param jags_output The JAGS output
#'
#' @return Summary of the parameter estimates
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' input_data <- reslr_load(data = data)
#' parameter_estimate(jags_output = reslr_mcmc(input_data = input_data, model_type = "eiv_slr_t"))
parameter_estimate <- function(jags_output) {
  mu_pred <- .lower <- .upper <- x <- variable <- sd <- mad <- q5 <- q95 <- pred_y <- `w.m[1]`<- `w.m[50]`<- lwr_95 <- upr_95 <- alpha <- cp <- sigma_g <- phi <- sigma <- mu_x <- dat <- NULL

  m <- jags_output$noisy_model_run_output$BUGSoutput$sims.matrix
  sample_draws <- tidybayes::tidy_draws(m)
  n_iter <- sample_draws$.iteration %>%
    unique() %>%
    length()
  # If the user sets iteration value extremely high and to save time reduce it
  if (jags_output$noisy_model_run_output$n.iter > 10000) {
    sample_draws <- sample_draws %>% dplyr::slice_sample(n = 1000)
    n_iterations <- 1000
  }
  jags_data <- jags_output$jags_data

  if (inherits(jags_output, "eiv_slr_t") == TRUE) {
    # Output from mcmc------------------------
    mu_post_pred <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    # Adaptive UI
    # Dataframes for plotting output---------------
    total_model_df <- data.frame(
      RSL_mod = apply(mu_post_pred, 2, mean),
      RSL_mod_upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      jags_output$predict_data$linear_rate,
      jags_output$predict_data$linear_rate_err,
      ID = "Total Posterior Model"
    )
    names(total_model_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName",
      "linear_rate", "linear_rate_err",
      "ID"
    )

    output_dataframes <- list(total_model_df = total_model_df)

    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c("alpha", "beta")) %>%
      dplyr::mutate(
        par_mean = mean, #* mod$scale_factor_y,
        par_mean = mean, #* mod$scale_factor_y,
        par_sd = sd, #* mod$scale_factor_y,
        par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
        par_q5 = q5, #* mod$scale_factor_y,
        par_q95 = q95 # * mod$scale_factor_y
      )
  }

  if (inherits(jags_output, "eiv_cp1_t") == TRUE) {
    # Output from mcmc------------------------
    mu_post_pred <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    # Adaptive UI
    # Dataframes for plotting output---------------
    total_model_df <- data.frame(
      RSL_mod = apply(mu_post_pred, 2, mean),
      RSL_mod_upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      jags_output$predict_data$linear_rate,
      jags_output$predict_data$linear_rate_err,
      ID = "Total Posterior Model"
    )
    names(total_model_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName", "linear_rate", "linear_rate_err",
      "ID"
    )
    # Output dataframes for plots
    output_dataframes <- list(total_model_df = total_model_df)
    # Estimated parameters
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c("alpha", "beta[1]", "beta[2]", "cp", "sigma"))
  }

  if (inherits(jags_output, "eiv_cp2_t") == TRUE) {
    # Output from mcmc------------------------
    mu_post_pred <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    # Adaptive UI
    # Dataframes for plotting output---------------
    total_model_df <- data.frame(
      RSL_mod = apply(mu_post_pred, 2, mean),
      RSL_mod_upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      jags_output$predict_data$linear_rate,
      jags_output$predict_data$linear_rate_err,
      ID = "Total Posterior Model"
    )
    names(total_model_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName", "linear_rate", "linear_rate_err",
      "ID"
    )
    # Output dataframes for plots
    output_dataframes <- list(total_model_df = total_model_df)
    # Estimated parameters
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "alpha[1]", "alpha[2]",
        "beta[1]", "beta[2]",
        "beta[3]", "cp[1]",
        "cp[2]", "sigma"
      ))
  }

  if (inherits(jags_output, "eiv_cp3_t") == TRUE) {
    # Output from mcmc------------------------
    mu_post_pred <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    # Adaptive UI
    # Dataframes for plotting output---------------
    total_model_df <- data.frame(
      RSL_mod = apply(mu_post_pred, 2, mean),
      RSL_mod_upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      jags_output$predict_data$linear_rate,
      jags_output$predict_data$linear_rate_err,
      ID = "Total Posterior Model"
    )
    names(total_model_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName", "linear_rate", "linear_rate_err",
      "ID"
    )
    # Output dataframes for plots
    output_dataframes <- list(total_model_df = total_model_df)
    # Estimated parameters
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "alpha[1]", "alpha[2]",
        "alpha[3]", "beta[1]", "beta[2]",
        "beta[3]", "beta[4]", "cp[1]",
        "cp[2]", "cp[3]", "sigma"
      ))
  }

  if (inherits(jags_output, "eiv_igp_t") == TRUE) {
    # Get predictions on a grid of t values.
    Ngrid <- jags_data$Ngrid
    tgrid <- jags_data$tstar
    tstar <- jags_data$tstar

    Dist <- jags_data$Dist

    # Set up the matrix that will contain the estimates
    pred <- matrix(NA, ncol = Ngrid, nrow = n_iter)
    K.gw <- K <- K.w.inv <- array(NA, c(n_iter, Ngrid, Ngrid))

    ######## Initialize quadrature for the integration########
    L <- 30 ## this sets the precision of the integration quadrature (higher is better but more computationally expensive)
    index <- 1:L
    cosfunc <- cos(((2 * index - 1) * pi) / (2 * L))

    quad1 <- array(dim = c(nrow = Ngrid, ncol = Ngrid, L))
    quad2 <- array(dim = c(nrow = Ngrid, ncol = Ngrid, L))

    for (j in 1:Ngrid)
    {
      for (k in 1:Ngrid)
      {
        quad1[k, j, ] <- abs((tgrid[k] * cosfunc / 2) + (tgrid[k] / 2) - tstar[j])^1.99
        quad2[k, j, ] <- ((tgrid[k] / 2) * (pi / L)) * (sqrt(1 - cosfunc^2))
      }
    }

    # Get posterior samples of rates
    w.ms <- as.matrix(sample_draws %>% dplyr::select(`w.m[1]`:`w.m[50]`))
    # Get estimates
    for (i in 1:n_iter) {
      for (k in 1:Ngrid) {
        for (j in 1:Ngrid) {
          K.gw[i, j, k] <- sum((sample_draws$phi[i]^quad1[j, k, ]) * quad2[j, k, ]) #### Quadrature function
        } # End j loop
      } # End k loop

      K[i, , ] <- sample_draws$phi[i]^(Dist^1.99)
      K.w.inv[i, , ] <- solve(K[i, , ])
      pred[i, ] <- sample_draws$alpha[i] + K.gw[i, , ] %*% K.w.inv[i, , ] %*% w.ms[i, ]
    } # End i loop
    # pred <- pred * mod$scale_factor_y
    # w.ms <- (w.ms * mod$scale_factor_y) / mod$scale_factor_x

    # if (mod$BP_scale) w.ms <- -1 * w.ms# Are we missing brackets here?

    # Output dataframes for plots
    output_dataframes <- dplyr::tibble(
      # Should this be predict data instead?
      t = seq(min(jags_output$data$Age), max(jags_output$data$Age), length.out = 50),
      pred_y = apply(pred, 2, mean),
      lwr_95 = apply(pred, 2, stats::quantile, probs = 0.025),
      upr_95 = apply(pred, 2, stats::quantile, probs = 0.975),
      rate_y = apply(w.ms, 2,mean),
      rate_lwr_95 = apply(w.ms, 2, stats::quantile, probs = 0.025),
      rate_upr_95 = apply(w.ms, 2, stats::quantile, probs = 0.975)
    )
    # Estimated parameters
    mean_samps <- apply(w.ms, 1, mean)
    w_summary <- dplyr::tibble(
      variable = "overall_rate",
      mean = mean(mean_samps), sd = stats::sd(mean_samps),
      mad = stats::mad(mean_samps), q5 = stats::quantile(mean_samps, 0.05),
      q95 = stats::quantile(mean_samps, 0.95), rhat = NA, ess_bulk = NA, ess_tail = NA
    )
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c("phi", "sigma_g", "sigma"))
    par_summary <- rbind(par_summary, w_summary)
  }

  if (inherits(jags_output, "ni_spline_t") == TRUE) {
    # Output from mcmc------------------------
    mu_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_y
    mu_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_deriv
    mu_post_pred <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    mu_pred_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred_deriv

    # Dataframes for plotting output using prediction grid---------------
    total_model_df <- data.frame(
      # RSL_mod = apply(mu_post, 2, mean),
      # RSL_mod_upr = apply(mu_post, 2, stats::quantile, probs = 0.025),
      # RSL_mod_lwr = apply(mu_post, 2, stats::quantile, probs = 0.975),
      # upr_50 = apply(mu_post, 2, stats::quantile, probs = 0.25),
      # lwr_50 = apply(mu_post, 2, stats::quantile, probs = 0.75),
      # jags_output$data$Age,
      # jags_output$data$SiteName,
      RSL_mod = apply(mu_post_pred, 2, mean),
      RSL_mod_upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      jags_output$predict_data$linear_rate,
      jags_output$predict_data$linear_rate_err,
      ID = "Total Predicted Posterior Model"
    )
    names(total_model_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName", "linear_rate", "linear_rate_err",
      "ID"
    )
    # Dataframes for derivative plots from prediction grids------------
    total_model_rate_df <- data.frame(
      # RSL_mod = apply(mu_deriv_post, 2, mean),
      # RSL_mod_upr = apply(mu_deriv_post, 2, stats::quantile, probs = 0.025),
      # RSL_mod_lwr = apply(mu_deriv_post, 2, stats::quantile, probs = 0.975),
      # upr_50 = apply(mu_deriv_post, 2, stats::quantile, probs = 0.25),
      # lwr_50 = apply(mu_deriv_post, 2, stats::quantile, probs = 0.75),
      # jags_output$data$Age,
      # jags_output$data$SiteName,
      RSL_mod = apply(mu_pred_deriv_post, 2, mean),
      RSL_mod_upr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      jags_output$predict_data$linear_rate,
      jags_output$predict_data$linear_rate_err,
      ID = "Rate of Change of Posterior Model"
    )
    names(total_model_rate_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName", "linear_rate", "linear_rate_err",
      "ID"
    )

    # Output dataframes for plotting
    output_dataframes <- list(
      total_model_df = total_model_df,
      total_model_rate_df = total_model_rate_df
    )

    # Output parameter estimates
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c("b_t", "r", "sigma_t", "sigma_res")) %>%
      dplyr::mutate(
        par_mean = mean, #* mod$scale_factor_y,
        par_mean = mean, #* mod$scale_factor_y,
        par_sd = sd, #* mod$scale_factor_y,
        par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
        par_q5 = q5, #* mod$scale_factor_y,
        par_q95 = q95 # * mod$scale_factor_y
      )
  }

  if (inherits(jags_output, "ni_spline_st") == TRUE) {
    # Output from mcmc------------------------
    mu_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_y
    mu_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_deriv
    mu_post_pred <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    mu_pred_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred_deriv


    # Dataframes for plotting output using prediction grid---------------
    total_model_df <- data.frame(
      RSL_mod = apply(mu_post_pred, 2, mean),
      RSL_mod_upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      jags_output$predict_data$linear_rate,
      jags_output$predict_data$linear_rate_err,
      ID = "Total Predicted Posterior Model"
    )
    names(total_model_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName", "linear_rate", "linear_rate_err",
      "ID"
    )

    # Dataframes for derivative plots from prediction grids------------
    total_model_rate_df <- data.frame(
      RSL_mod = apply(mu_pred_deriv_post, 2, mean),
      RSL_mod_upr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      jags_output$predict_data$linear_rate,
      jags_output$predict_data$linear_rate_err,
      ID = "Rate of Change of Posterior Model"
    )
    names(total_model_rate_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName", "linear_rate", "linear_rate_err",
      "ID"
    )

    # Output data frames for plotting
    output_dataframes <- list(
      total_model_df = total_model_df,
      total_model_rate_df = total_model_rate_df
    )

    # Output parameter estimates
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c("b_st", "sigma_st", "sigma_res")) %>%
      dplyr::mutate(
        par_mean = mean, #* mod$scale_factor_y,
        par_mean = mean, #* mod$scale_factor_y,
        par_sd = sd, #* mod$scale_factor_y,
        par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
        par_q5 = q5, #* mod$scale_factor_y,
        par_q95 = q95 # * mod$scale_factor_y
      )
  }

  if (inherits(jags_output, "ni_gam_decomp") == TRUE) {
    # Output from mcmc------------------------
    mu_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_y
    # mu_deriv_post <- cbind(jags_output$data$Age,t(mu_post))
    # mu_deriv_post_test <- diff(mu_deriv_post[,1])/diff(mu_deriv_post[,1])

    mu_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_deriv


    mu_post_pred <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    mu_pred_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred_deriv

    time_component_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$r
    time_deriv_component_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$r_deriv
    time_component_pred_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$r_pred
    time_component_pred_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$r_pred_deriv

    g_h_component_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$g_h_z_x
    g_h_component_pred_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$g_h_z_x_pred

    space_time_component_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$l
    space_time_component_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$l_deriv
    space_time_component_pred_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$l_pred
    space_time_component_pred_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$l_pred_deriv


    # Dataframes for plotting output---------------
    total_model_df <- data.frame(
      RSL_mod = apply(mu_post, 2, mean),
      RSL_mod_upr = apply(mu_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(mu_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_post, 2, stats::quantile, probs = 0.75),
      jags_output$data$Age,
      jags_output$data$SiteName,
      jags_output$data$linear_rate,
      jags_output$data$linear_rate_err,
      ID = "Total Posterior Model",
      data_type_id = jags_output$data$data_type_id
    )
    names(total_model_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName", "linear_rate", "linear_rate_err",
      "ID","data_type_id"
    )
    # # Derivative approx
    # total_model_rate_df_deriv <-
    #   #find the average x between 2 points
    #    # Age[-1]-diff(Age)/2)
    #     diff(total_model_df$RSL)/diff(total_model_df$Age)

    # Dataframes for derivative plots------------
    total_model_rate_df <- data.frame(
      RSL_mod = apply(mu_deriv_post, 2, mean),
      RSL_mod_upr = apply(mu_deriv_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(mu_deriv_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_deriv_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_deriv_post, 2, stats::quantile, probs = 0.75),
      jags_output$data$Age,
      jags_output$data$SiteName,
      ID = "Rate of Change of Posterior Model",
      data_type_id = jags_output$data$data_type_id
    )
    names(total_model_rate_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName",
      "ID","data_type_id"
    )

    # Dataframes for plotting output using prediction grid---------------
    mod_output_pred_df <- data.frame(
      RSL_mod = apply(mu_post_pred, 2, mean),
      RSL_mod_upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      jags_output$predict_data$linear_rate,
      jags_output$predict_data$linear_rate_err,
      ID = "Total Predicted Posterior Model",
      data_type_id = jags_output$predict_data$data_type_id
    )
    names(mod_output_pred_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName","linear_rate", "linear_rate_err",
      "ID","data_type_id"
    )
    # Dataframes for derivative plots from prediction grids------------
    mod_output_pred_deriv_df <- data.frame(
      RSL_mod = apply(mu_pred_deriv_post, 2, mean),
      RSL_mod_upr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      ID = "Rate of Change of Posterior Model",
      data_type_id = jags_output$predict_data$data_type_id
    )
    names(mod_output_pred_deriv_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName",
      "ID","data_type_id"
    )


    # Regional Component: Spline in Time-----------------------
    time_post_component_df <- data.frame(
      RSL_mod = apply(time_component_post, 2, mean),
      RSL_mod_upr = apply(time_component_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(time_component_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(time_component_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(time_component_post, 2, stats::quantile, probs = 0.75),
      jags_output$data$Age,
      jags_output$data$SiteName,
      ID = "Regional Component",
      data_type_id = jags_output$data$data_type_id
    )
    names(time_post_component_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName",
      "ID","data_type_id"
    )
    # Derivative Regional Component: Spline in Time-----------------------
    time_deriv_component_post_df <- data.frame(
      RSL_mod = apply(time_deriv_component_post, 2, mean),
      RSL_mod_upr = apply(time_deriv_component_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(time_deriv_component_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(time_deriv_component_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(time_deriv_component_post, 2, stats::quantile, probs = 0.75),
      jags_output$data$Age,
      jags_output$data$SiteName,
      ID = "Derivative Regional Component",
      data_type_id = jags_output$data$data_type_id
    )
    names(time_deriv_component_post_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName",
      "ID","data_type_id"
    )

    # Regional Component using Prediction: Spline in Time-----------------------
    time_post_pred_component_df <- data.frame(
      RSL_mod = apply(time_component_pred_post, 2, mean),
      RSL_mod_upr = apply(time_component_pred_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(time_component_pred_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(time_component_pred_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(time_component_pred_post, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      ID = "Regional Component",
      data_type_id = jags_output$predict_data$data_type_id
    )
    names(time_post_pred_component_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName",
      "ID","data_type_id"
    )

    # Derivative Regional Component using Prediction: Spline in Time-----------------------
    time_post_pred_deriv_component_df <- data.frame(
      RSL_mod = apply(time_component_pred_deriv_post, 2, mean),
      RSL_mod_upr = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      ID = "Derivative Regional Component",
      data_type_id = jags_output$predict_data$data_type_id
    )
    names(time_post_pred_deriv_component_df) <- c(
      "RSL", "upr", "lwr", "upr_50", "lwr_50",
      "Age",
      "SiteName",
      "ID","data_type_id"
    )


    # Linear Local Component + Site-Specific vertical offset------------------
    g_h_component_post_df <- data.frame(
      RSL_mod = apply(g_h_component_post, 2, mean),
      RSL_mod_upr = apply(g_h_component_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(g_h_component_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(g_h_component_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(g_h_component_post, 2, stats::quantile, probs = 0.75),
      jags_output$data$Age,
      jags_output$data$SiteName,
      jags_output$data$linear_rate,
      jags_output$data$linear_rate_err,
      ID = "Linear Local Component and site-specific vertical offset",
      data_type_id = jags_output$data$data_type_id
    )
    names(g_h_component_post_df) <- c(
      "RSL",
      "upr",
      "lwr",
      "upr_50",
      "lwr_50",
      "Age",
      "SiteName", "linear_rate", "linear_rate_err",
      "ID",
      "data_type_id"
    )

    # Linear Local Component + Site-Specific vertical offset for prediction grid------------------
    g_h_component_pred_post_df <- data.frame(
      RSL_mod = apply(g_h_component_pred_post, 2, mean),
      RSL_mod_upr = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      jags_output$predict_data$linear_rate,
      jags_output$predict_data$linear_rate_err,
      ID = "Linear Local Component and site-specific vertical offset",
       data_type_id = jags_output$predict_data$data_type_id
    )
    names(g_h_component_pred_post_df) <- c(
      "RSL",
      "upr",
      "lwr",
      "upr_50",
      "lwr_50",
      "Age",
      "SiteName", "linear_rate", "linear_rate_err",
      "ID","data_type_id"
    )

    # Non-Linear Local Component ---------------------------------
    space_time_component_post_df <- data.frame(
      RSL_mod = apply(space_time_component_post, 2, mean),
      RSL_mod_upr = apply(space_time_component_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(space_time_component_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(space_time_component_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(space_time_component_post, 2, stats::quantile, probs = 0.75),
      jags_output$data$Age,
      jags_output$data$SiteName,
      ID = "Non-Linear Local Component",
      data_type_id = jags_output$data$data_type_id
    )
    names(space_time_component_post_df) <- c(
      "RSL",
      "upr",
      "lwr",
      "upr_50",
      "lwr_50",
      "Age",
      "SiteName",
      "ID","data_type_id"
    )

    # Non-Linear Local Component Derivative---------------------------------
    space_time_component_deriv_post_df <- data.frame(
      RSL_mod = apply(space_time_component_deriv_post, 2, mean),
      RSL_mod_upr = apply(space_time_component_deriv_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(space_time_component_deriv_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(space_time_component_deriv_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(space_time_component_deriv_post, 2, stats::quantile, probs = 0.75),
      jags_output$data$Age,
      jags_output$data$SiteName,
      ID = "Rate of Change of Non-Linear Local Component",
      data_type_id = jags_output$data$data_type_id
    )
    names(space_time_component_deriv_post_df) <- c(
      "RSL",
      "upr",
      "lwr",
      "upr_50",
      "lwr_50",
      "Age",
      "SiteName",
      "ID",
      "data_type_id"
    )
    # Non-Linear Local Component on Prediction grid---------------------------------
    space_time_component_pred_post_df <- data.frame(
      RSL_mod = apply(space_time_component_pred_post, 2, mean),
      RSL_mod_upr = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      ID = "Non-Linear Local Component",
      data_type_id = jags_output$predict_data$data_type_id
    )
    names(space_time_component_pred_post_df) <- c(
      "RSL",
      "upr",
      "lwr",
      "upr_50",
      "lwr_50",
      "Age",
      "SiteName",
      "ID",
      "data_type_id"
    )
    # Non-Linear Local Component Derivative---------------------------------
    space_time_component_pred_deriv_post_df <- data.frame(
      RSL_mod = apply(space_time_component_pred_deriv_post, 2, mean),
      RSL_mod_upr = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.025),
      RSL_mod_lwr = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.75),
      jags_output$predict_data$Age,
      jags_output$predict_data$SiteName,
      ID = "Rate of Change of Non-Linear Local Component",
      data_type_id = jags_output$predict_data$data_type_id
    )
    names(space_time_component_pred_deriv_post_df) <- c(
      "RSL",
      "upr",
      "lwr",
      "upr_50",
      "lwr_50",
      "Age",
      "SiteName",
      "ID",
      "data_type_id"
    )

    # Output dataframes for plots
    output_dataframes <- list(
      total_model_df = total_model_df,
      total_model_rate_df = total_model_rate_df,
      mod_output_pred_df=mod_output_pred_df,
      mod_output_pred_deriv_df=mod_output_pred_deriv_df,

      time_post_component_df = time_post_component_df,
      time_deriv_component_post_df = time_deriv_component_post_df,
      time_post_pred_component_df = time_post_pred_component_df,
      time_post_pred_deriv_component_df = time_post_pred_deriv_component_df,
      g_h_component_post_df = g_h_component_post_df,
      g_h_component_pred_post_df= g_h_component_pred_post_df,
      space_time_component_post_df = space_time_component_post_df,
      space_time_component_deriv_post_df = space_time_component_deriv_post_df,
      space_time_component_pred_post_df = space_time_component_pred_post_df,
      space_time_component_pred_deriv_post_df = space_time_component_pred_deriv_post_df
    )

    # Estimated parameter summaries
    par_summary <- posterior::summarise_draws(sample_draws) %>%
      dplyr::filter(variable %in% c(
        "b_t", "r", "g_h_z_x",
        "b_st", "l", "sigma_st",
        "sigma_t", "sigma_res"
      )) %>%
      dplyr::mutate(
        par_mean = mean, #* mod$scale_factor_y,
        par_mean = mean, #* mod$scale_factor_y,
        par_sd = sd, #* mod$scale_factor_y,
        par_mad = mad, #* mod$scale_factor_y, # WHAT this one?
        par_q5 = q5, #* mod$scale_factor_y,
        par_q95 = q95 # * mod$scale_factor_y
      )
  }


  return(list(
    output_dataframes = output_dataframes,
    par_summary = par_summary
  ))
}
