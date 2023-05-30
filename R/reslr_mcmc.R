#' Run a \code{reslr_input} object through the main reslr Markov chain Monte Carlo (MCMC) function using a chosen statistical model
#'
#' In this function, a variety of statistical models can be run depending on the requirements of the user. All models are written within a Bayesian framework and use JAGS (Just Another Gibbs Sampler) to calculate Markov Chain Monte Carlo (MCMC) simulation to obtain estimates of unknown parameters. The user has the ability to alter the number of iterations, the number of burnin, the number of chains and the thinning. These options relate to the amount of MCMC simulations required and should be reviewed by the user to ensure model convergence is achieved without excessively long run times.
#' The user chooses their 'model_type' and as a range of models to choose from.
#'
#' @param input_data Input data from the \code{reslr_load} function
#' @param model_type The user selects their statistical model type. The user can select a Errors in Variable Simple Linear Regression using "eiv_slr_t". The user can select a Errors in Variable Change Point Regression using  "eiv_cp_t". The user can select a Errors in Variable Integrated Gaussian Process using "eiv_igp_t". The user can select a Noisy Input Spline in Time using "ni_spline_t". The user can select a Noisy Input Spline in Space Time using "ni_spline_st". The user can select a Noisy Input Generalised Additive Model using "ni_gam_decomp".
#' @param n_iterations Number of iterations. Increasing this value will increase the computational run time.
#' @param n_burnin Size of burn-in. This number removes a certain number of samples at the beginning.
#' @param n_thin Amount of thinning.
#' @param n_chains Number of MCMC chains. The number of times the model will be run.
#' @param igp_smooth This setting is focused on the Errors in Variables Integrated Gaussian Process model. It informs the prior for the smoothness (correlation) parameter if model = "igp" is chosen. Choose a value between 0 and 1. Closer to 1 will increase smoothness.
#' @param n_cp This setting is focused on the Errors in Variables Change Point model. The user can select the number of change points 1,2 or 3.
#' @param CI Size of the credible interval required by the user. The default is 0.95 corresponding to 95%.
#' @param spline_nseg This setting is focused on the Noisy Input Spline model. It provides the number of segments used to create basis functions.
#' @param spline_nseg_t This setting is focused on the Noisy Input Generalised Additive Model. It provides the number of segments used to create basis functions.
#' @param spline_nseg_st This setting is focused on the Noisy Input Generalised Additive Model. It provides the number of segments used to create basis functions.

#' @return A list containing the input data, the JAGS output and output dataframes used for final plots.
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' input_data <- reslr_load(data = data)
#' reslr_mcmc(input_data = input_data, model_type = "eiv_slr_t")
reslr_mcmc <- function(input_data,
                       model_type,
                       n_cp = 1,
                       igp_smooth = 0.2,
                       n_iterations = 5000,
                       n_burnin = 1000,
                       n_thin = 4,
                       n_chains = 3,
                       CI = 0.95,
                       spline_nseg = NULL,
                       spline_nseg_t = 20,
                       spline_nseg_st = 6
) {
  UseMethod("reslr_mcmc")
}

#' @export
reslr_mcmc.reslr_input <- function(input_data,
                                   model_type,
                                   n_cp = 1,
                                   igp_smooth = 0.2,
                                   n_iterations = 5000,
                                   n_burnin = 1000,
                                   n_thin = 4,
                                   n_chains = 3,
                                   CI = 0.95,
                                   spline_nseg = NULL,
                                   spline_nseg_t = 20,
                                   spline_nseg_st = 6
) {
  Age <- RSL <- Age_err <- RSL_err <- SiteName <- Longitude <- Latitude <- max_Age <- min_Age <- linear_rate <- linear_rate_err <- NULL

  # Input Data -------------
  data <- input_data$data %>%
    dplyr::mutate(Age = Age/1000, Age_err = Age_err/1000)
  data_grid <- input_data$data_grid %>%
    dplyr::mutate(Age = Age/1000)

  # Simple Linear Regression ----------------
  if (model_type == "eiv_slr_t") {
    # JAGS file
    jags_file <- system.file("jags_models", "model_eiv_slr_t.jags", package = "reslr")

    # Parameters to save in JAGS-----------------
    jags_pars <- c(
      "mu_pred",
      "beta",
      "alpha",
      "sigma_y"
    )

    # JAGS data----------------------
    jags_data <- list(
      y = data$RSL,
      y_err = data$RSL_err,
      t_err = data$Age_err,
      t_pred = data_grid$Age,
      t = data$Age,
      n_obs = nrow(data),
      n_pred = nrow(data_grid)
    )

    # Run JAGS------------------------
    model_run <-
      R2jags::jags(
        data = jags_data,
        parameters.to.save = jags_pars,
        model.file = jags_file,
        n.iter = n_iterations,
        n.burnin = n_burnin,
        n.thin = n_thin,
        n.chains = n_chains
      )
    # Convert back to 1000
    data <- data %>%
      dplyr::mutate(Age = Age*1000, Age_err = Age_err*1000)
    data_grid <- data_grid %>%
      dplyr::mutate(Age = Age*1000)


    # Output from mcmc & dataframes for plots
    output_dataframes <- create_output_df(
      noisy_model_run_output = model_run,
      data_grid,
      rate_grid = FALSE,
      decomposition = FALSE,
      CI = CI
    )


    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = model_run,
      jags_data = jags_data,
      data = data,
      data_grid = data_grid,
      output_dataframes = output_dataframes
    )

    # Classing the JAGS output in NIGAM time--------------
    class(jags_output) <- c("reslr_output", "eiv_slr_t")
  }

  # 1 Change Point Model-------------------
  if (model_type == "eiv_cp_t" & n_cp == 1) {
    # JAGS file
    jags_file <- system.file("jags_models", "model_eiv_cp1_t.jags", package = "reslr")

    # JAGS parameters to save
    jags_pars <- c(
      "mu_pred",
      "beta",
      "alpha",
      "sigma_y",
      "cp"
    )

    # JAGS data
    jags_data <- list(
      y = data$RSL,
      y_err = data$RSL_err,
      t = data$Age,
      n_pred = nrow(data_grid),
      t_pred = data_grid$Age,
      t_err = data$Age_err,
      t_min = min(data$Age),
      t_max = max(data$Age),
      n_obs = nrow(data)
    )

    # Run JAGS------------------------
    model_run <- suppressWarnings(R2jags::jags(
      data = jags_data,
      parameters.to.save = jags_pars,
      model.file = jags_file,
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains
    ))
    # Convert back to 1000
    data <- data %>%
      dplyr::mutate(Age = Age*1000, Age_err = Age_err*1000)
    data_grid <- data_grid %>%
      dplyr::mutate(Age = Age*1000)

    # Output from mcmc & dataframes for plots
    output_dataframes <- create_output_df(
      noisy_model_run_output = model_run,
      data_grid,
      rate_grid = FALSE,
      decomposition = FALSE,
      CI = CI
    )
    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = model_run,
      jags_data = jags_data,
      data = data,
      data_grid = data_grid,
      output_dataframes = output_dataframes
    )

    # Classing the JAGS output in 1 Change Point--------------
    class(jags_output) <- c("reslr_output", "eiv_cp1_t")
  }


  # 2 Change Point Model----------------------
  if (model_type == "eiv_cp_t" & n_cp == 2) {
    # JAGS file
    jags_file <- system.file("jags_models", "model_eiv_cp2_t.jags", package = "reslr")

    # Initial functions for Change point required
    myinitial <- function() {
      list(
        "alpha" = c(stats::rnorm(2, 0, 3)),
        "beta" = c(stats::rnorm(1, 0, 3), NA, stats::rnorm(1, 0, 3)),
        "cp.temp" = c(stats::runif(2, min(data$Age), max(data$Age)))
      )
    }


    # JAGS parameters to save
    jags_pars <- c(
      "mu_pred",
      "beta",
      "alpha",
      "sigma_y",
      "cp"
    )

    # JAGS data
    jags_data <- list(
      y = data$RSL,
      y_err = data$RSL_err,
      t = data$Age,
      n_pred = nrow(data_grid),
      t_pred = data_grid$Age,
      t_err = data$Age_err,
      t_min = min(data$Age),
      t_max = max(data$Age),
      n_obs = nrow(data)
    )

    # Run JAGS------------------------
    model_run <- suppressWarnings(R2jags::jags(
      data = jags_data,
      parameters.to.save = jags_pars,
      model.file = jags_file,
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains,
      inits = myinitial
    ))
    # Convert back to 1000
    data <- data %>%
      dplyr::mutate(Age = Age*1000, Age_err = Age_err*1000)
    data_grid <- data_grid %>%
      dplyr::mutate(Age = Age*1000)

    # Output from mcmc & dataframes for plots
    output_dataframes <- create_output_df(
      noisy_model_run_output = model_run,
      data_grid,
      rate_grid = FALSE,
      decomposition = FALSE,
      CI = CI
    )

    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = model_run,
      jags_data = jags_data,
      data = data,
      data_grid = data_grid,
      output_dataframes = output_dataframes
    )

    # Classing the JAGS output in 2 Change Point--------------
    class(jags_output) <- c("reslr_output", "eiv_cp2_t")
  }

  # 3 Change Point Model----------------------
  if (model_type == "eiv_cp_t" & n_cp == 3) {
    # JAGS file
    jags_file <- system.file("jags_models", "model_eiv_cp3_t.jags", package = "reslr")

    # Initial functions for Change point required
    myinitial <- function() {
      list(
        "alpha" = c(stats::rnorm(3, 0, 3)),
        "beta" = c(stats::rnorm(1, 0, 3), NA, NA, stats::rnorm(1, 0, 3)),
        "cp.temp" = c(stats::runif(3, min(data$Age), max(data$Age)))
      )
    }


    # JAGS parameters to save
    jags_pars <- c(
      "mu_pred",
      "beta",
      "alpha",
      "sigma_y",
      "cp"
    )

    # JAGS data
    jags_data <- list(
      y = data$RSL,
      y_err = data$RSL_err,
      t = data$Age,
      n_pred = nrow(data_grid),
      t_pred = data_grid$Age,
      t_err = data$Age_err,
      t_min = min(data$Age),
      t_max = max(data$Age),
      n_obs = nrow(data)
    )

    # Run JAGS------------------------
    model_run <- suppressWarnings(R2jags::jags(
      data = jags_data,
      parameters.to.save = jags_pars,
      model.file = jags_file,
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains,
      inits = myinitial
    ))
    # Convert back to 1000-------
    data <- data %>%
      dplyr::mutate(Age = Age*1000, Age_err = Age_err*1000)
    data_grid <- data_grid %>%
      dplyr::mutate(Age = Age*1000)

    # Output from mcmc & dataframes for plots
    output_dataframes <- create_output_df(
      noisy_model_run_output = model_run,
      data_grid,
      rate_grid = FALSE,
      decomposition = FALSE,
      CI = CI
    )

    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = model_run,
      jags_data = jags_data,
      data = data,
      data_grid = data_grid,
      output_dataframes = output_dataframes
    )

    # Classing the JAGS output in 3 Change Point--------------
    class(jags_output) <- c("reslr_output", "eiv_cp3_t")
  }

  # Errors-in-Variables Integrated Gaussian Process------------------
  if (model_type == "eiv_igp_t") {
    # JAGS file
    jags_file <- system.file("jags_models", "model_eiv_igp_t.jags", package = "reslr")
    # JAGS parameters to save
    jags_pars <- c(
      "rho",
      "nu",
      "sigma_y",
      "w.m",
      "alpha",
      "beta"
    )
    # Detrended data---
    if (inherits(input_data, "detrend_data") == TRUE) {
      # JAGS data
      igp_dat_list <- igp_detrend_data(data, data_grid)

      jags_data <- list(
        y = data$SL,
        y_err = data$RSL_err,
        t = data$Age,
        n_pred = nrow(data_grid),
        t_pred = data_grid$Age,
        t_err = data$Age_err,
        t_min = min(data$Age),
        t_max = max(data$Age),
        n_obs = nrow(data),
        al = igp_smooth * 10 / (1 - igp_smooth)
      )
      jags_data <- c(igp_dat_list, jags_data)

      # Run JAGS------------------------
      model_run <- suppressWarnings(R2jags::jags(
        data = jags_data,
        parameters.to.save = jags_pars,
        model.file = jags_file,
        n.iter = n_iterations,
        n.burnin = n_burnin,
        n.thin = n_thin,
        n.chains = n_chains
      ))
      # Convert back to 1000
      data <- data %>%
        dplyr::mutate(Age = Age*1000, Age_err = Age_err*1000)
      data_grid <- data_grid %>%
        dplyr::mutate(Age = Age*1000)

      # Output dataframe for plots
      output_dataframes <- create_igp_output_df(
        model_run = model_run,
        jags_data = jags_data,
        data_grid = data_grid,
        CI = CI
      )
      # Output with everything-------------
      jags_output <- list(
        noisy_model_run_output = model_run, # Watch this
        jags_data = jags_data,
        data = data,
        data_grid = data_grid,
        output_dataframes = output_dataframes,
        detrend_data_un_box = input_data$detrend_data_un_box
      )
      # Classing the JAGS output for eiv_igp_t--------------
      class(jags_output) <- c("reslr_output", "eiv_igp_t", "detrend_data")
    }

    # No detrended data
    else {
      # JAGS data
      igp_dat_list <- igp_data(data, data_grid)
      jags_data <- list(
        y = data$RSL,
        y_err = data$RSL_err,
        t = data$Age,
        n_pred = nrow(data_grid),
        t_pred = data_grid$Age,
        t_err = data$Age_err,
        t_min = min(data$Age),
        t_max = max(data$Age),
        n_obs = nrow(data),
        al = igp_smooth * 10 / (1 - igp_smooth)
      )
      jags_data <- c(igp_dat_list, jags_data)

      # Run JAGS------------------------
      model_run <- suppressWarnings(R2jags::jags(
        data = jags_data,
        parameters.to.save = jags_pars,
        model.file = jags_file,
        n.iter = n_iterations,
        n.burnin = n_burnin,
        n.thin = n_thin,
        n.chains = n_chains
      ))

      # Convert back to 1000
      data <- data %>%
        dplyr::mutate(Age = Age*1000, Age_err = Age_err*1000)
      data_grid <- data_grid %>%
        dplyr::mutate(Age = Age*1000)

      # Output dataframe for plots
      output_dataframes <- create_igp_output_df(
        model_run = model_run,
        jags_data = jags_data,
        data_grid = data_grid,
        CI = CI
      )

      # Output with everything-------------
      jags_output <- list(
        noisy_model_run_output = model_run, # Watch this
        jags_data = jags_data,
        data = data,
        data_grid = data_grid,
        output_dataframes = output_dataframes
      )

      # Classing the JAGS output for eiv_igp_t--------------
      class(jags_output) <- c("reslr_output", "eiv_igp_t")
    }
  }


  # Noisy Input spline in time-----------------------------------
  if (model_type == "ni_spline_t") {
    # No Noise
    jags_file <- system.file("jags_models", "model_ni_spline_t.jags", package = "reslr")

    # Parameters to save in JAGs-----------------
    jags_pars <- c(
      "mu_y",
      "sigma_y",
      "b_t",
      "r",
      "sigma_beta",
      "sigmasq_all",
      "residuals"
    )

    # Basis functions in time -----------------------------
    spline_basis_fun_list <- spline_basis_fun(
      data = data,
      data_grid = data_grid,
      model_type = model_type,
      spline_nseg = spline_nseg,
      xl = xl,
      xr = xr
    )

    # JAGS data----------------------
    jags_data <- list(
      y = data$RSL,
      y_err = data$RSL_err,
      t = data$Age,
      n_obs = nrow(data),
      B_t = spline_basis_fun_list$B_t,
      n_knots_t = ncol(spline_basis_fun_list$B_t)
    )

    # Run JAGS------------------------
    model_run <- suppressWarnings(R2jags::jags(
      data = jags_data,
      parameters.to.save = jags_pars,
      model.file = jags_file,
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains
    ))
    # Adding Noisy Input-------------------
    update_input_df <- add_noisy_input(
      data = data,
      data_grid = data_grid,
      model_run = model_run,
      model_type = model_type,
      jags_data = jags_data,
      spline_nseg = spline_nseg
    )
    data <- update_input_df$data
    data_grid <- update_input_df$data_grid
    # Include Noise-----------------------
    if("CV_fold" %in% colnames(data_grid)){
      noisy_jags_file <- system.file("jags_models", "noisy_model_ni_spline_t_valid.jags", package = "reslr")
    }
    else{
      noisy_jags_file <- system.file("jags_models", "noisy_model_ni_spline_t.jags", package = "reslr")
    }

    # Parameters to save in JAGs-----------------
    jags_pars <- c(
      "mu_y",
      "mu_deriv",
      "mu_pred",
      "mu_pred_deriv",
      "r_pred_deriv",
      "sigma_y",
      "b_t",
      "r",
      "r_deriv",
      "sigma_beta",
      "sigmasq_all",
      "sigmasq_all_pred",
      "y_pred",
      "r_pred",
      "residuals"
    )
    if("CV_fold" %in% colnames(data_grid)){
      # JAGS data for second model run-----------
      jags_data <- list(
        NI_var_term = data$NI_var_term,
        NI_var_grid_term = data_grid$NI_var_grid_term,
        y = data$RSL,
        y_err = data$RSL_err,
        y_err_grid = data_grid$RSL_err,
        t = data$Age,
        n_obs = nrow(data),
        t_pred = data_grid$Age,
        n_pred = length(data_grid$Age),
        B_t = spline_basis_fun_list$B_t,
        B_t_deriv = spline_basis_fun_list$B_t_deriv,
        B_t_pred = spline_basis_fun_list$B_t_pred,
        n_knots_t = ncol(spline_basis_fun_list$B_t),
        B_t_pred_deriv = spline_basis_fun_list$B_t_pred_deriv
      )
    }
    else{
    # JAGS data for second model run-----------
    jags_data <- list(
      NI_var_term = data$NI_var_term,
      y = data$RSL,
      y_err = data$RSL_err,
      t = data$Age,
      n_obs = nrow(data),
      t_pred = data_grid$Age,
      n_pred = length(data_grid$Age),
      B_t = spline_basis_fun_list$B_t,
      B_t_deriv = spline_basis_fun_list$B_t_deriv,
      B_t_pred = spline_basis_fun_list$B_t_pred,
      n_knots_t = ncol(spline_basis_fun_list$B_t),
      B_t_pred_deriv = spline_basis_fun_list$B_t_pred_deriv
    )}
    # Run JAGS--------------
    noisy_model_run_output <-
      suppressWarnings(R2jags::jags(
        data = jags_data,
        parameters.to.save = jags_pars,
        model.file = noisy_jags_file,
        n.iter = n_iterations,
        n.burnin = n_burnin,
        n.thin = n_thin,
        n.chains = n_chains
      ))
    # Convert back to 1000
    data <- data %>%
      dplyr::mutate(Age = Age*1000, Age_err = Age_err*1000)
    data_grid <- data_grid %>%
      dplyr::mutate(Age = Age*1000)

    # Output from mcmc & dataframes for plots
    output_dataframes <- create_output_df(noisy_model_run_output,
      data_grid = data_grid,
      rate_grid = TRUE,
      decomposition = FALSE,
      CI = CI
    )

    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = noisy_model_run_output,
      jags_data = jags_data,
      data = data,
      data_grid = data_grid,
      output_dataframes = output_dataframes
    )

    # Classing the JAGS output in NIGAM time--------------
    class(jags_output) <- c("reslr_output", "ni_spline_t")

  }

  # Noisy Input GAM in Space Time-------------------------------------------
  if (model_type == "ni_spline_st") {
    jags_file <- system.file("jags_models", "model_ni_spline_st.jags", package = "reslr")
    # Basis functions in space time -----------------------------
    spline_basis_fun_list <- spline_basis_fun(
      data = data,
      data_grid = data_grid,
      model_type = model_type,
      spline_nseg = spline_nseg,
      xl = xl,
      xr = xr
    )

    # JAGS data
    jags_data <- list(
      y = data$RSL,
      y_err = data$RSL_err,
      t = data$Age,
      site = as.factor(data$SiteName),
      n_sites = length(unique(data$SiteName)),
      n_obs = nrow(data),
      B_st = spline_basis_fun_list$B_st,
      n_knots_st = ncol(spline_basis_fun_list$B_st)
    )

    # Parameters to save in JAGs
    jags_pars <- c(
      "mu_y",
      "sigma_y",
      "b_st",
      "l",
      "sigma_beta",
      "sigmasq_all"
    )

    # Run JAGS------------------------
    model_run <- suppressWarnings(R2jags::jags(
      data = jags_data,
      parameters.to.save = jags_pars,
      model.file = jags_file,
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains
    ))

    # Adding Noisy Input-------------------
    update_input_df <- add_noisy_input(
      data = data,
      data_grid = data_grid,
      model_run = model_run,
      model_type = model_type,
      jags_data = jags_data,
      spline_nseg = spline_nseg
    )
    data <- update_input_df$data
    data_grid <- update_input_df$data_grid
    # Include Noise-----------------------
    if("CV_fold" %in% colnames(data_grid)){
      noisy_jags_file <- system.file("jags_models", "noisy_model_ni_spline_st_valid.jags", package = "reslr")
    }
    else{
      noisy_jags_file <- system.file("jags_models", "noisy_model_ni_spline_st.jags", package = "reslr")
    }

    # JAGS input data
    jags_data <- list(
      y = data$RSL,
      NI_var_term = data$NI_var_term,
      NI_var_grid_term = data_grid$NI_var_grid_term,
      y_err = data$RSL_err,
      y_err_grid = data_grid$RSL_err,
      t = data$Age,
      n_pred = length(data_grid$Age),
      n_obs = nrow(data),
      B_st = spline_basis_fun_list$B_st,
      B_st_deriv = spline_basis_fun_list$B_st_deriv,
      B_st_pred = spline_basis_fun_list$B_st_pred,
      B_st_deriv_pred = spline_basis_fun_list$B_st_deriv_pred,
      n_knots_st = ncol(spline_basis_fun_list$B_st)
    )

    # Parameters to save in JAGs
    jags_pars <- c(
      "mu_y",
      "mu_pred",
      "mu_deriv",
      "mu_pred_deriv",
      "sigma_y",
      "b_st",
      "l",
      "l_pred",
      "sigma_beta",
      "sigmasq_all",
      "residuals",
      "sigmasq_all_pred",
      "y_pred"
    )

    # Run JAGS--------------
    noisy_model_run_output <-
      suppressWarnings(R2jags::jags(
        data = jags_data,
        parameters.to.save = jags_pars,
        model.file = noisy_jags_file,
        n.iter = n_iterations,
        n.burnin = n_burnin,
        n.thin = n_thin,
        n.chains = n_chains
      ))

    # Convert back to 1000
    data <- data %>%
      dplyr::mutate(Age = Age*1000, Age_err = Age_err*1000)
    data_grid <- data_grid %>%
      dplyr::mutate(Age = Age*1000)

    # Output from mcmc & dataframes for plots
    output_dataframes <- create_output_df(noisy_model_run_output,
      data_grid = data_grid,
      rate_grid = TRUE,
      decomposition = FALSE,
      CI = CI
    )

    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = noisy_model_run_output,
      jags_data = jags_data,
      data = data,
      data_grid = data_grid,
      output_dataframes = output_dataframes
    )

    # Classing the JAGS output in NIGAM space time--------------
    class(jags_output) <- c("reslr_output", "ni_spline_st")

  }

  # Noisy Input GAM for decomposition of RSL signal-------------------------------------------
  if (model_type == "ni_gam_decomp") {
    # jags file
    jags_file <- system.file("jags_models", "model_ni_gam_decomp.jags", package = "reslr")

    # Basis functions in space time -----------------------------
    spline_basis_fun_list <- spline_basis_fun(
      data = data,
      data_grid = data_grid,
      model_type = model_type,
      spline_nseg_t = spline_nseg_t,
      spline_nseg_st = spline_nseg_st,
      xl = xl,
      xr = xr
    )

    # JAGS data
    jags_data <- list(
      y = data$RSL,
      y_err = data$RSL_err,
      t = data$Age,
      t_pred = data_grid$Age,
      site = as.factor(data$SiteName),
      site_pred = as.factor(data_grid$SiteName),
      n_sites = length(unique(data$SiteName)),
      n_site_pred = length(unique(data_grid$SiteName)),
      n_obs = nrow(data),
      n_pred = nrow(data_grid),
      B_t = spline_basis_fun_list$B_t,
      B_t_pred = spline_basis_fun_list$B_t_pred,
      n_knots_t = ncol(spline_basis_fun_list$B_t),
      linear_rate = data %>%
        dplyr::group_by(SiteName) %>%
        dplyr::slice(1) %>%
        dplyr::select(linear_rate) %>%
        dplyr::pull(),
      linear_rate_err = data %>%
        dplyr::group_by(SiteName) %>%
        dplyr::slice(1) %>%
        dplyr::select(linear_rate_err) %>%
        dplyr::pull(),
      linear_rate_pred = data_grid %>%
        dplyr::group_by(SiteName) %>%
        dplyr::slice(1) %>%
        dplyr::select(linear_rate) %>%
        dplyr::pull(),
      linear_rate_err_pred = data_grid %>%
        dplyr::group_by(SiteName) %>%
        dplyr::slice(1) %>%
        dplyr::select(linear_rate_err) %>%
        dplyr::pull()
      )

    # Parameters to save in JAGs
    jags_pars <- c(
      "mu_y",
      "sigma_y",
      "b_t",
      "h_z_x",
      "g_h_z_x",
      "g_z_x",
      "r",
      "intercept",
      "sigma_beta_h",
      "sigma_beta_r",
      "sigma_beta_l",
      "b_g",
      "sigmasq_all"
    )

    # Run JAGS------------------------
    model_run <- suppressWarnings(R2jags::jags(
      data = jags_data,
      parameters.to.save = jags_pars,
      model.file = jags_file,
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains
    ))

    # Adding Noisy Input-------------------
    update_input_df <- add_noisy_input(
      data = data,
      data_grid = data_grid,
      model_run = model_run,
      model_type = model_type,
      jags_data = jags_data,
      spline_nseg_t = spline_nseg_t,
      spline_nseg_st = spline_nseg_st
    )
    data <- update_input_df$data
    data_grid <- update_input_df$data_grid
    # Include Noise-----------------------
    if("CV_fold" %in% colnames(data_grid)){
      noisy_jags_file <- system.file("jags_models", "noisy_model_ni_gam_decomp_valid.jags", package = "reslr")
    }
    else{
      noisy_jags_file <- system.file("jags_models", "noisy_model_ni_gam_decomp.jags", package = "reslr")
    }


    # JAGS input data
    jags_data <- list(
      NI_var_term = data$NI_var_term,
      NI_var_grid_term = data_grid$NI_var_grid_term,
      b_t_value = model_run$BUGSoutput$median$b_t,
      b_t_sd_value = model_run$BUGSoutput$sd$b_t,
      h_value = model_run$BUGSoutput$median$intercept,
      h_sd_value = model_run$BUGSoutput$sd$intercept,
      y = data$RSL,
      y_err = data$RSL_err,
      y_err_grid = data_grid$RSL_err,
      t = data$Age,
      t_pred = data_grid$Age,
      site = as.factor(data$SiteName),
      site_pred = as.factor(data_grid$SiteName),
      n_sites = length(unique(data$SiteName)),
      n_sites_pred = length(unique(data_grid$SiteName)),
      n_pred = length(data_grid$Age),
      n_obs = nrow(data),
      B_t = spline_basis_fun_list$B_t,
      B_t_deriv = spline_basis_fun_list$B_t_deriv,
      B_t_pred = spline_basis_fun_list$B_t_pred,
      B_t_pred_deriv = spline_basis_fun_list$B_t_pred_deriv,
      n_knots_t = ncol(spline_basis_fun_list$B_t),
      B_st = spline_basis_fun_list$B_st,
      B_st_pred = spline_basis_fun_list$B_st_pred,
      B_st_deriv = spline_basis_fun_list$B_st_deriv,
      B_st_deriv_pred = spline_basis_fun_list$B_st_deriv_pred,
      n_knots_st = ncol(spline_basis_fun_list$B_st),
      linear_rate = data %>%
        dplyr::group_by(SiteName) %>%
        dplyr::slice(1) %>%
        dplyr::select(linear_rate) %>%
        dplyr::pull(),
      linear_rate_err = data %>%
        dplyr::group_by(SiteName) %>%
        dplyr::slice(1) %>%
        dplyr::select(linear_rate_err) %>%
        dplyr::pull(),
      linear_rate_pred = data_grid %>%
        dplyr::group_by(SiteName) %>%
        dplyr::slice(1) %>%
        dplyr::select(linear_rate) %>%
        dplyr::pull(),
      linear_rate_err_pred = data_grid %>%
        dplyr::group_by(SiteName) %>%
        dplyr::slice(1) %>%
        dplyr::select(linear_rate_err) %>%
        dplyr::pull()
    )

    # Parameters to save in JAGs
    jags_pars <- c(
      "mu_y",
      "mu_deriv",
      "mu_pred",
      "mu_pred_deriv",
      "sigma_y",
      "sigma_beta_h",
      "sigma_beta_r",
      "sigma_beta_l",
      "sigmasq_all",
      "b_t",
      "b_st",
      "b_g",
      "intercept",
      "h_z_x",
      "h_z_x_pred",
      "g_h_z_x",
      "g_h_z_x_pred",
      "g_z_x",
      "g_z_x_deriv",
      "g_z_x_pred",
      "g_z_x_pred_deriv",
      "r",
      "r_deriv",
      "r_pred",
      "r_pred_deriv",
      "l",
      "l_deriv",
      "l_pred",
      "l_pred_deriv",
      "residuals",
      "sigmasq_all_pred",
      "y_pred"
    )
    # Run JAGS--------------
    noisy_model_run_output <-
      suppressWarnings(R2jags::jags(
        data = jags_data,
        parameters.to.save = jags_pars,
        model.file = noisy_jags_file,
        n.iter = n_iterations,
        n.burnin = n_burnin,
        n.thin = n_thin,
        n.chains = n_chains
      ))
    # Convert back to 1000
    data <- data %>%
      dplyr::mutate(Age = Age*1000, Age_err = Age_err*1000)
    data_grid <- data_grid %>%
      dplyr::mutate(Age = Age*1000)

    # Output from mcmc & dataframes for plots
    output_dataframes <- create_output_df(noisy_model_run_output,
      data_grid = data_grid,
      rate_grid = TRUE,
      decomposition = TRUE,
      CI = CI
    )

    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = noisy_model_run_output,
      model_run_output = model_run,
      jags_data = jags_data,
      data = data,
      data_grid = data_grid,
      output_dataframes = output_dataframes
    )

    # Classing the JAGS output in NIGAM for RSL decomposition--------------
    class(jags_output) <- c("reslr_output", "ni_gam_decomp")
  }
  return(jags_output)
}
