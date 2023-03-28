#' Function to create dataframe for plotting using IGP results
#'
#' @param model_run_output  The JAGS output
#' @param jags_data Data associated with IGP data
#' @param data_grid Input data grid
#' @noRd
create_igp_output_df <- function(model_run,jags_data,data_grid){
  m <- model_run$BUGSoutput$sims.matrix
  sample_draws <- tidybayes::tidy_draws(m)
  n_iter <- sample_draws$.iteration %>%
    unique() %>%
    length()
  # If the user sets iteration value extremely high and to save time reduce it
  if (model_run$n.iter > 10000) {
    sample_draws <- sample_draws %>% dplyr::slice_sample(n = 1000)
    n_iterations <- 1000
  }
  jags_data <- jags_data
  # Get predictions on a grid of t values.
  Ngrid <- jags_data$Ngrid
  tgrid <- jags_data$tstar
  tstar <- jags_data$tstar
  Dist <- jags_data$Dist

  # Set up the matrix that will contain the estimates
  pred_full <- matrix(NA, ncol = Ngrid, nrow = n_iter)
  K.gw <- K <- K.w.inv <- array(NA, c(n_iter, Ngrid, Ngrid))

  ######## Initialize quadrature for the integration########
  L <- 30 ## this sets the precision of the integration quadrature (higher is better but more computationally expensive)
  index <- 1:L
  cosfunc <- cos(((2 * index - 1) * pi) / (2 * L))

  quad1 <- array(dim = c(nrow = Ngrid, ncol = Ngrid, L))
  quad2 <- array(dim = c(nrow = Ngrid, ncol = Ngrid, L))

  for (j in 1:Ngrid) {
    for (k in 1:Ngrid) {
      quad1[k, j, ] <- abs((tgrid[k] * cosfunc / 2) + (tgrid[k] / 2) - tstar[j])^1.99
      quad2[k, j, ] <- ((tgrid[k] / 2) * (pi / L)) * (sqrt(1 - cosfunc^2))
    }
  }


  # Get posterior samples of rates
  w.ms <- as.matrix(model_run$BUGSoutput$sims.list$w.m)

  # Get estimates
  for (i in 1:n_iter) {
    for (k in 1:Ngrid) {
      for (j in 1:Ngrid) {
        K.gw[i, j, k] <- sum((sample_draws$phi[i]^quad1[j, k, ]) * quad2[j, k, ]) #### Quadrature function
      } # End j loop
    } # End k loop

    K[i, , ] <- sample_draws$phi[i]^(Dist^1.99)
    K.w.inv[i, , ] <- solve(K[i, , ])
    pred_full[i, ] <- sample_draws$alpha[i] + K.gw[i, , ] %*% K.w.inv[i, , ] %*% w.ms[i, ]
  } # End i loop
  # pred_full <- pred_full * mod$scale_factor_y
  # w.ms <- (w.ms * mod$scale_factor_y) / mod$scale_factor_x

  # Output dataframes for plots
  output_dataframes <- dplyr::tibble(
    data_grid,
    pred = apply(pred_full, 2, mean),
    lwr_95 = apply(pred_full, 2, stats::quantile, probs = 0.025),
    upr_95 = apply(pred_full, 2, stats::quantile, probs = 0.975),
    upr_50 = apply(pred_full, 2, stats::quantile, probs = 0.25),
    lwr_50 = apply(pred_full, 2, stats::quantile, probs = 0.75),
    rate_pred = apply(w.ms, 2, mean),
    rate_lwr_95 = apply(w.ms, 2, stats::quantile, probs = 0.025),
    rate_upr_95 = apply(w.ms, 2, stats::quantile, probs = 0.975),
    rate_upr_50 = apply(w.ms, 2, stats::quantile, probs = 0.25),
    rate_lwr_50 = apply(w.ms, 2, stats::quantile, probs = 0.75)
  )
  return(output_dataframes)
}


#' Function to create the dataframes for plotting
#'
#' @param noisy_model_run_output The JAGS output
#' @param rate_grid If rate of change is included in the dataframe
#' @param decomposition Is the full model decomposition included in dataframe
#' @noRd
create_output_df <- function(noisy_model_run_output,
                             data_grid,#jags_output,
                             rate_grid = FALSE,
                             decomposition = FALSE) {
  # mu_post_pred <- noisy_model_run_output$BUGSoutput$sims.list$mu_pred
  # output_dataframes <- data.frame(
  #   #jags_output$data_grid,
  #   data_grid,
  #   pred = apply(mu_post_pred, 2, mean),
  #   upr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
  #   lwr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
  #   upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
  #   lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75))
  # ID = "Total Posterior Model"
  if (rate_grid == TRUE) {
    mu_post_pred <- noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    output_dataframes <- data.frame(
      #jags_output$data_grid,
      data_grid,
      pred = apply(mu_post_pred, 2, mean),
      upr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      lwr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75))
    # ID = "Total Posterior Model"

    #mu_pred_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred_deriv
    mu_pred_deriv_post <- noisy_model_run_output$BUGSoutput$sims.list$mu_pred_deriv
    output_dataframes <- data.frame(
      output_dataframes,
      rate_pred =  apply(mu_pred_deriv_post, 2, mean),
      rate_upr_95 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.025),
      rate_lwr_95 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.975),
      rate_upr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.25),
      rate_lwr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.75)
    )
  } else {
    #mu_post_pred <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    mu_post_pred <- noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    output_dataframes <- data.frame(
      #jags_output$data_grid,
      data_grid,
      pred = apply(mu_post_pred, 2, mean),
      upr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      lwr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75))
    # ID = "Total Posterior Model"
    output_dataframes <- output_dataframes
  }

  if (decomposition == TRUE & rate_grid == TRUE) {
    # Total Component from JAGS output
    mu_post_pred <- noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    total_model_fit_df <- data.frame(
      data_grid,
      pred = apply(mu_post_pred, 2, mean),
      upr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      lwr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
      ID = "Total Posterior Model")

    mu_pred_deriv_post <- noisy_model_run_output$BUGSoutput$sims.list$mu_pred_deriv
    total_model_rate_df <-
      data.frame(
        data_grid,
        rate_pred =  apply(mu_pred_deriv_post, 2, mean),
        rate_upr_95 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.025),
        rate_lwr_95 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.975),
        rate_upr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.25),
        rate_lwr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.75),
        ID = "Total Rate of Change for Posterior Model")
    # Regional component
    time_component_pred_post <- noisy_model_run_output$BUGSoutput$sims.list$r_pred
    regional_component_df <- data.frame(
      data_grid,
      pred = apply(time_component_pred_post, 2, mean),
      upr_95 = apply(time_component_pred_post, 2, stats::quantile, probs = 0.025),
      lwr_95 = apply(time_component_pred_post, 2, stats::quantile, probs = 0.975),
      upr_50 = apply(time_component_pred_post, 2, stats::quantile, probs = 0.25),
      lwr_50 = apply(time_component_pred_post, 2, stats::quantile, probs = 0.75),
      ID = "Regional Component")

    time_component_pred_deriv_post <- noisy_model_run_output$BUGSoutput$sims.list$r_pred_deriv
    regional_rate_component_df <-
      data.frame(
      data_grid,
      rate_pred =  apply(time_component_pred_deriv_post, 2, mean),
      rate_upr_95 = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.025),
      rate_lwr_95 = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.975),
      rate_upr_50 = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.25),
      rate_lwr_50 = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.75),
      ID = "Rate of Change for Regional Component")

    # Vertical Offset & Linear Local Component
    g_h_component_pred_post <- noisy_model_run_output$BUGSoutput$sims.list$g_h_z_x_pred
    lin_loc_component_df <-
      data.frame(
        data_grid,
        pred = apply(g_h_component_pred_post, 2, mean),
        upr_95 = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.025),
        lwr_95 = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.975),
        upr_50 = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.25),
        lwr_50 = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.75),
        ID = "Site Specific vertical offset + \n Linear Local Component")

    # Non linear local component
    space_time_component_pred_post <- noisy_model_run_output$BUGSoutput$sims.list$l_pred
    non_lin_loc_component_df <-
      data.frame(
        data_grid,
        pred = apply(space_time_component_pred_post, 2, mean),
        upr_95 = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.025),
        lwr_95 = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.975),
        upr_50 = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.25),
        lwr_50 = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.75),
        ID = "Non Linear Local Component")
    space_time_component_pred_deriv_post <- noisy_model_run_output$BUGSoutput$sims.list$l_pred_deriv
    non_lin_loc_rate_component_df <-
      data.frame(
        data_grid,
        rate_pred =  apply(space_time_component_pred_deriv_post, 2, mean),
        rate_upr_95 = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.025),
        rate_lwr_95 = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.975),
        rate_upr_50 = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.25),
        rate_lwr_50 = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.75),
        ID = "Rate of Change for Non Linear Local Component")


    output_dataframes <- list(
      total_model_fit_df = total_model_fit_df,
      total_model_rate_df = total_model_rate_df,
      regional_component_df = regional_component_df,
      regional_rate_component_df = regional_rate_component_df,
      lin_loc_component_df = lin_loc_component_df,
      non_lin_loc_component_df=non_lin_loc_component_df,
      non_lin_loc_rate_component_df = non_lin_loc_rate_component_df
    )
  }
  # else {
  #   mu_post_pred <- noisy_model_run_output$BUGSoutput$sims.list$mu_pred
  #   output_dataframes <- data.frame(
  #     #jags_output$data_grid,
  #     data_grid,
  #     pred = apply(mu_post_pred, 2, mean),
  #     upr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
  #     lwr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
  #     upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
  #     lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75))
  #   # ID = "Total Posterior Model"
  #   output_dataframes <- output_dataframes
  # }

  return(output_dataframes)
}
#' Adding Noisy Input to the dataframe
#'
#' @param model_run JAGS output
#' @param model_type NIGAM in time or space time or the full decomposition
#' @param data Input data
#' @noRd
add_noisy_input <- function(model_run, model_type, data) {
  if (model_type == "ni_spline_t") {
    #-----Get posterior samples for SL-----
    b_t_post <- model_run$BUGSoutput$sims.list$b_t

    pred_mean_calc <- function(t_new) {
      # Create the regional basis functions
      B_deriv_t <- bs_bbase(t_new,
        xl = min(data$Age),
        xr = max(data$Age))
      # # Create the regional basis functions
      # B_deriv_t_old <- bs_bbase(t_new,
      #                       xl = min(data$Age),
      #                       xr = max(data$Age))
      # #--------New Create the differencing matrix------
      # D_new_t <- diff(diag(ncol(B_deriv_t_old)), diff = 2)#diff = 2)
      # Q_new_t <- t(D_new_t) %*% solve(D_new_t %*% t(D_new_t))
      # #Z_new_t <- B_deriv_t_old %*% Q_new_t
      # B_deriv_t <- B_deriv_t_old %*% Q_new_t
      #B_deriv_t <- B_deriv_t_old

      #----Deriv----
      return(B_deriv_t %*% colMeans(b_t_post))
    }
    #-------Now create derivatives----
    h <- 0.001
    t <- data$Age
    deriv <- (pred_mean_calc(t + h) - pred_mean_calc(t - h)) / (2 * h)
  }

  if (model_type == "ni_spline_st") {
    b_st_post <- model_run$BUGSoutput$sims.list$b_st

    pred_mean_calc <- function(t_new) {
      B_time <- bs_bbase(t_new,
        xl = min(data$Age),
        xr = max(data$Age)
      )
      B_space_1 <- bs_bbase(data$Latitude,
        xl = min(data$Latitude),
        xr = max(data$Latitude)
      )
      B_space_2 <- bs_bbase(data$Longitude,
        xl = min(data$Longitude),
        xr = max(data$Longitude)
      )
      B_l_deriv_full <- matrix(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data)
      )
      regional_knots_loc <- rep(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1)
      )
      count <- 1
      for (i in 1:ncol(B_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_l_deriv_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero
      B_l_deriv <- B_l_deriv_full[, -which(colSums(B_l_deriv_full) < 0.1)]
      return(B_l_deriv %*% colMeans(b_st_post))
    }
    #-------Now create derivatives----
    h <- 0.001
    t <- data$Age
    deriv <- (pred_mean_calc(t + h) - pred_mean_calc(t - h)) / (2 * h)
  }

  if (model_type == "ni_gam_decomp") {
    #-----Get posterior samples for SL-----
    intercept_post <- model_run$BUGSoutput$sims.list$intercept
    b_t_post <- model_run$BUGSoutput$sims.list$b_t
    b_g_post <- model_run$BUGSoutput$sims.list$b_g

    pred_mean_calc <- function(t_new) {
      # Create the regional basis functions
      B_t <- bs_bbase(t_new,
        xl = min(data$Age),
        xr = max(data$Age),
        nseg = 5
      )
      #----Deriv----
      return(intercept_post[data$SiteName] + B_t %*% colMeans(b_t_post) + b_g_post[data$SiteName] * (t_new))
    }
    #-------Now create derivatives----
    h <- 0.01
    t <- data$Age
    deriv <- (pred_mean_calc(t + h) - pred_mean_calc(t - h)) / (2 * h)
  }

  # Add this new term in - this is the extra standard deviation on each term----
  data$NI_var_term <- sqrt(deriv^2 %*% data$Age_err^2)[, 1]

  # Writing new dataframe with noisy extra column------
  data <- data.frame(data)
  return(data)
}


#' Correcting data from EIV-IGP model
#'
#' @param data Input data
#' @noRd

igp_data <- function(data, data_grid) {
  Age <- RSL <- Longitude <- Latitude <- SiteName <- NULL
  ############# Set up the grid for the GP ###################
  tgrid <- data_grid$Age
  Ngrid <- length(tgrid)

  ### Change data to lower zero for integration
  min_t <- min(data$Age)
  t <- data$Age - min_t
  tstar <- tgrid - min_t

  Dist <- fields::rdist(tstar) ### Distance matrix required for the model
  D <- cbind(t, data$RSL) ### Combine the x,y data for the model

  ######## Initialize quadrature for the integration########
  N <- nrow(data)
  L <- 30 ## this sets the precision of the integration quadrature (higher is better but more computationally expensive)
  index <- 1:L
  cosfunc <- cos(((2 * index - 1) * pi) / (2 * L))

  quad1 <- array(dim = c(nrow = N, ncol = Ngrid, L))
  quad2 <- array(dim = c(nrow = N, ncol = Ngrid, L))

  for (j in 1:Ngrid)
  {
    for (k in 1:N)
    {
      quad1[k, j, ] <- abs((t[k] * cosfunc / 2) + (t[k] / 2) - tstar[j])^1.99
      quad2[k, j, ] <- ((t[k] / 2) * (pi / L)) * (sqrt(1 - cosfunc^2))
    }
  }


  return(list(
    tstar = tstar,
    N = N,
    Ngrid = Ngrid,
    Dist = Dist,
    quad1 = quad1,
    quad2 = quad2,
    cosfunc = cosfunc,
    ppi = pi,
    L = L
  ))
}

#' Creating basis function for splines
#'
#' @param data Input data
#' @param data_grid Prediction data
#' @param model_type Type of model
#' @noRd


spline_basis_fun <- function(data, data_grid, model_type) {
  Age <- RSL <- Longitude <- Latitude <- SiteName <- NULL

  if (model_type == "ni_spline_t") {
    t <- data$Age
    # Basis functions in time for data-----------------------
    B_t<- bs_bbase(t, xl = min(t), xr = max(t))
    # B_t_old <- bs_bbase(t, xl = min(t), xr = max(t))
    # #--------Create the differencing matrix for spline in time------
    # D_t <- diff(diag(ncol(B_t_old)), diff = 2)#2)
    # Q_t <- t(D_t) %*% solve(D_t %*% t(D_t))
    # #Z_t <- B_t_old %*% Q_t
    # B_t <- B_t_old %*% Q_t
    # B_t <- B_t_old

    # Finding derivative  of basis functions using first principals-----------
    first_deriv_calc <- function(t_new) {
      # Create the regional basis functions
      B_t <- bs_bbase(t_new,
        xl = min(data$Age),
        xr = max(data$Age))
      # # Create the regional basis functions
      # B_t_old <- bs_bbase(t_new,
      #                 xl = min(data$Age),
      #                 xr = max(data$Age))
      # #--------Create the differencing matrix for spline in time------
      # D_t <- diff(diag(ncol(B_t_old)), diff = 2)#diff = 2)
      # Q_t <- t(D_t) %*% solve(D_t %*% t(D_t))
      # #Z_t <- B_t_old %*% Q_t
      # B_t <- B_t_old %*% Q_t
      # B_t <- B_t_old
      return(B_t)
    }
    # Now create derivatives----------------------
    # h <- 0.001
    h <- 0.01
    first_deriv_step1 <- first_deriv_calc(t + h)
    first_deriv_step2 <- first_deriv_calc(t - h)
    B_t_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)

    # Basis functions in time using prediction data frame-----------------------
    t_pred <- sort(data_grid$Age)
    B_t_pred <- bs_bbase(t_pred,
      xl = min(t), xr = max(t)
    )
    # B_t_pred_old <- bs_bbase(t_pred,
    #                      xl = min(t), xr = max(t)
    # )
    # #--------Create the differencing matrix for spline in time------
    # D_t <- diff(diag(ncol(B_t_old)),diff = 2)# diff = 2)
    # Q_t <- t(D_t) %*% solve(D_t %*% t(D_t))
    # #Z_t <- B_t_old %*% Q_t
    # B_t_pred <- B_t_pred_old %*% Q_t
    # B_t_pred <- B_t_pred_old

    # Now create derivatives----------------------
    # h <- 0.001
    h <- 0.01
    t_pred <- data_grid$Age
    first_deriv_step1 <- first_deriv_calc(t_pred + h)
    first_deriv_step2 <- first_deriv_calc(t_pred - h)
    B_t_pred_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)

    spline_basis_fun_list <- list(
      B_t = B_t,
      B_t_deriv = B_t_deriv,
      B_t_pred = B_t_pred,
      B_t_pred_deriv = B_t_pred_deriv
    )
  }

  if (model_type == "ni_spline_st") {
    t <- data$Age
    # Basis functions in space time for data-----------------------
    B_time <- bs_bbase(t,
      xl = min(t),
      xr = max(t)
    )
    B_space_1 <- bs_bbase(data$Latitude,
      xl = min(data$Latitude),
      xr = max(data$Latitude)
    )
    B_space_2 <- bs_bbase(data$Longitude,
      xl = min(data$Longitude),
      xr = max(data$Longitude)
    )

    B_st_full <- matrix(NA,
      ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
      nrow = nrow(data)
    )
    regional_knots_loc <- rep(NA,
      ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1)
    )
    count <- 1
    for (i in 1:ncol(B_time)) {
      for (j in 1:ncol(B_space_1)) {
        for (k in 1:ncol(B_space_2)) {
          regional_knots_loc[count] <- i
          B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
          count <- count + 1
        }
      }
    }

    # Get rid of all the columns which are just zero
    B_st <- B_st_full[, -which(colSums(B_st_full) < 0.1)]

    # Find the index here that you remove then use this in the derivative
    remove_col_index <- which(colSums(B_st_full) < 0.1)

    first_deriv_calc <- function(t_new) {
      # Now the local basis functions
      B_time <- bs_bbase(t_new,
        xl = min(data$Age),
        xr = max(data$Age)
      )
      B_space_1 <- bs_bbase(data$Latitude,
        xl = min(data$Latitude),
        xr = max(data$Latitude)
      )
      B_space_2 <- bs_bbase(data$Longitude,
        xl = min(data$Longitude),
        xr = max(data$Longitude)
      )

      B_st_full <- matrix(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data)
      )
      regional_knots_loc <- rep(NA, ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1))
      count <- 1
      for (i in 1:ncol(B_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero
      B_st <- B_st_full[, -which(colSums(B_st_full) < 0.1)]
      return(B_st)
    }
    # Now create derivatives----
    h <- 0.001

    first_deriv_step1 <- first_deriv_calc(t + h)
    first_deriv_step2 <- first_deriv_calc(t - h)
    B_st_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)


    # Basis functions in space time using prediction data frame-----------------------
    B_pred_time <- bs_bbase(data_grid$Age,
      xl = min(data$Age),
      xr = max(data$Age)
    )
    B_space_1 <- bs_bbase(data_grid$Latitude,
      xl = min(data$Latitude),
      xr = max(data$Latitude)
    )
    B_space_2 <- bs_bbase(data_grid$Longitude,
      xl = min(data$Longitude),
      xr = max(data$Longitude)
    )

    suppressWarnings({
      B_st_pred_full <- matrix(NA,
        ncol = ncol(B_pred_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data_grid)
      )
      regional_knots_loc <- rep(NA,
        ncol = ncol(B_pred_time) * ncol(B_space_1) * ncol(B_space_1)
      )
      count <- 1
      for (i in 1:ncol(B_pred_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_pred_full[, count] <- B_pred_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero corresponding to the previous basis functions
      B_st_pred <- B_st_pred_full[, -remove_col_index]
    })
    # Now create derivatives for prediciton
    first_deriv_calc <- function(t_new) {
      # Now the local basis functions
      B_time <- bs_bbase(t_new,
        xl = min(data$Age),
        xr = max(data$Age)
      )
      B_space_1 <- bs_bbase(data_grid$Latitude,
        xl = min(data$Latitude),
        xr = max(data$Latitude)
      )
      B_space_2 <- bs_bbase(data_grid$Longitude,
        xl = min(data$Longitude),
        xr = max(data$Longitude)
      )

      B_st_full <- matrix(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data_grid)
      )
      regional_knots_loc <- rep(NA, ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1))
      count <- 1
      for (i in 1:ncol(B_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero
      # B_st <- B_st_full[,-which(colSums(B_st_full) < 0.1)]
      B_st <- B_st_full[, -remove_col_index]
      return(B_st)
    }
    h <- 0.001
    t_pred <- data_grid$Age

    first_deriv_step1 <- first_deriv_calc(t_pred + h)
    first_deriv_step2 <- first_deriv_calc(t_pred - h)
    B_st_deriv_pred <- (first_deriv_step1 - first_deriv_step2) / (2 * h)

    # All Basis Functions
    spline_basis_fun_list <- list(
      remove_col_index = remove_col_index,
      B_st = B_st,
      B_st_deriv = B_st_deriv,
      B_st_pred = B_st_pred,
      B_st_deriv_pred = B_st_deriv_pred
    )
  }

  if (model_type == "ni_gam_decomp") {
    # Basis functions in time for data-----------------------
    B_t <- bs_bbase(data$Age,
      xl = min(data$Age), xr = max(data$Age), nseg = 5
    )
    # Finding derivative  of basis functions using first principals-----------
    first_deriv_calc <- function(t_new) {
      # Create the regional basis functions
      B_t <- bs_bbase(t_new,
        xl = min(data$Age),
        xr = max(data$Age),
        nseg = 5
      ) # nseg = 20)
      return(B_t)
    }
    # Now create derivatives----------------------
    h <- 0.00001 # h <- 0.001
    t <- data$Age
    first_deriv_step1 <- first_deriv_calc(t + h)
    first_deriv_step2 <- first_deriv_calc(t - h)
    B_t_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)

    # Basis functions in time using prediction data frame-----------------------
    B_t_pred <- bs_bbase(data_grid$Age,
      xl = min(data$Age), xr = max(data$Age), nseg = 5
    )
    # Now create derivatives----------------------
    h <- 0.00001 # h <- 0.001
    t_pred <- data_grid$Age
    first_deriv_step1 <- first_deriv_calc(t_pred + h)
    first_deriv_step2 <- first_deriv_calc(t_pred - h)
    B_t_pred_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)


    # Basis functions in space time for data-----------------------
    B_time <- bs_bbase(data$Age,
      xl = min(data$Age),
      xr = max(data$Age)#,deg = 2, nseg = 6
    )
    B_space_1 <- bs_bbase(data$Latitude,
      xl = min(data$Latitude),
      xr = max(data$Latitude)#,deg = 2, nseg = 6
    )
    B_space_2 <- bs_bbase(data$Longitude,
      xl = min(data$Longitude),
      xr = max(data$Longitude)#,deg = 2, nseg = 6
    )

    B_st_full <- matrix(NA,
      ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
      nrow = nrow(data)
    )
    regional_knots_loc <- rep(NA,
      ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1)
    )
    count <- 1
    for (i in 1:ncol(B_time)) {
      for (j in 1:ncol(B_space_1)) {
        for (k in 1:ncol(B_space_2)) {
          regional_knots_loc[count] <- i
          B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
          count <- count + 1
        }
      }
    }

    # Get rid of all the columns which are just zero
    B_st <- B_st_full[, -which(colSums(B_st_full) < 0.1)]

    # Find the index here that you remove then use this in the derivative
    remove_col_index <- which(colSums(B_st_full) < 0.1)

    first_deriv_calc <- function(t_new) {
      # Now the local basis functions
      B_time <- bs_bbase(t_new,
        xl = min(data$Age),
        xr = max(data$Age)#,deg = 2, nseg = 6
        # deg = 2
      )
      B_space_1 <- bs_bbase(data$Latitude,
        xl = min(data$Latitude),
        xr = max(data$Latitude)#,deg = 2, nseg = 6
        # deg = 2
      )
      B_space_2 <- bs_bbase(data$Longitude,
        xl = min(data$Longitude),
        xr = max(data$Longitude)#,deg = 2, nseg = 6
        # deg = 2
      )

      B_st_full <- matrix(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data)
      )
      regional_knots_loc <- rep(NA, ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1))
      count <- 1
      for (i in 1:ncol(B_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero
      B_st <- B_st_full[, -which(colSums(B_st_full) < 0.1)]
      return(B_st)
    }
    #-------Now create derivatives----
    h <- 0.00001 # h <- 0.0001
    t <- data$Age

    first_deriv_step1 <- first_deriv_calc(t + h)
    first_deriv_step2 <- first_deriv_calc(t - h)
    B_st_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)


    # Basis functions in space time using prediction data frame-----------------------
    B_pred_time <- bs_bbase(data_grid$Age,
      xl = min(data$Age),
      xr = max(data$Age)#,deg = 2, nseg = 6
      # deg = 2
    )
    B_space_1 <- bs_bbase(data_grid$Latitude,
      xl = min(data$Latitude),
      xr = max(data$Latitude)#,deg = 2, nseg = 6
      # deg = 2
    )
    B_space_2 <- bs_bbase(data_grid$Longitude,
      xl = min(data$Longitude),
      xr = max(data$Longitude)#,deg = 2, nseg = 6
      # deg = 2
    )

    suppressWarnings({
      B_st_pred_full <- matrix(NA,
        ncol = ncol(B_pred_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data_grid)
      ) # Not sure here?? nrow(data)
      regional_knots_loc <- rep(NA,
        ncol = ncol(B_pred_time) * ncol(B_space_1) * ncol(B_space_1)
      )
      count <- 1
      for (i in 1:ncol(B_pred_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_pred_full[, count] <- B_pred_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero corresponding to the previous basis functions
      B_st_pred <- B_st_pred_full[, -remove_col_index]
    })
    #-------Now create derivatives for prediciton----
    first_deriv_calc <- function(t_new) {
      # Now the local basis functions
      B_time <- bs_bbase(t_new,
        xl = min(data$Age),
        xr = max(data$Age)#,deg = 2, nseg = 6
        # deg = 2
      )
      B_space_1 <- bs_bbase(data_grid$Latitude,
        xl = min(data$Latitude),
        xr = max(data$Latitude)#,deg = 2, nseg = 6
        # deg = 2
      )
      B_space_2 <- bs_bbase(data_grid$Longitude,
        xl = min(data$Longitude),
        xr = max(data$Longitude)#,deg = 2, nseg = 6
        # deg = 2
      )

      B_st_full <- matrix(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data_grid)
      ) # nrow(data))
      regional_knots_loc <- rep(NA, ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1))
      count <- 1
      for (i in 1:ncol(B_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero
      # B_st <- B_st_full[,-which(colSums(B_st_full) < 0.1)]
      B_st <- B_st_full[, -remove_col_index] # Not sure here
      return(B_st)
    }
    h <- 0.00001 # h <- 0.001
    t_pred <- data_grid$Age

    first_deriv_step1 <- first_deriv_calc(t_pred + h)
    first_deriv_step2 <- first_deriv_calc(t_pred - h)
    B_st_deriv_pred <- (first_deriv_step1 - first_deriv_step2) / (2 * h)

    # Derivative of Basis function for the total model fit-----------------
    first_deriv_calc <- function(t_new) {
      # Create the regional basis functions
      B_t <- bs_bbase(t_new,
        xl = min(data$Age),
        xr = max(data$Age)
      ) # nseg = 20)
      colnames(B_t) <- c(paste("B_t", 1:ncol(B_t), sep = ""))

      # Now the local basis functions
      B_time <- bs_bbase(t_new,
        xl = min(data$Age),
        xr = max(data$Age), deg = 2,nseg = 6
        # deg = 2
      )
      B_space_1 <- bs_bbase(data$Latitude,
        xl = min(data$Latitude),
        xr = max(data$Latitude), deg = 2,nseg = 6
        # deg = 2
      )
      B_space_2 <- bs_bbase(data$Longitude,
        xl = min(data$Longitude),
        xr = max(data$Longitude)#,deg = 2, nseg = 6
        # deg = 2
      )

      B_st_full <- matrix(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data)
      )
      regional_knots_loc <- rep(NA, ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1))
      count <- 1
      for (i in 1:ncol(B_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero
      B_st <- B_st_full[, -which(colSums(B_st_full) < 0.1)]
      colnames(B_st) <- c(paste("B_st", 1:ncol(B_st), sep = ""))
      # Dummy matrix for intercept & GIA
      B_h <- fastDummies::dummy_cols(data.frame(data$SiteName))
      B_h <- B_h[, -1]
      colnames(B_h) <- c(paste("B_h", 1:ncol(B_h), sep = ""))
      B_g <- B_h * t_new
      colnames(B_g) <- c(paste("B_g", 1:ncol(B_g), sep = ""))
      # Basis function matrix with B_local & B_regional
      output_B_tot <- cbind(
        B_h, B_g,
        B_t, B_st
      )


      return(output_B_tot)
    }
    #-------Now create derivatives----
    h <- 0.00001 # h <- 0.0001
    t <- data$Age

    first_deriv_step1 <- first_deriv_calc(t + h)
    first_deriv_step2 <- first_deriv_calc(t - h)
    B_tot_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)

    # New basis function for site specific vertical offset----------
    B_h_deriv <- as.matrix(B_tot_deriv[, grepl("B_h", names(B_tot_deriv))])
    # New Basis Functions for Random linear local component-Site Specific slope-------
    B_deriv_g_re <- as.matrix(B_tot_deriv[, grepl("B_g", names(B_tot_deriv))])
    # New Basis Functions for spline in time-------------------------
    B_deriv_t <- as.matrix(B_tot_deriv[, grepl("B_t", names(B_tot_deriv))])
    # New Basis Functions in Space time-------------------------------
    B_deriv_st <- as.matrix(B_tot_deriv[, grepl("B_st", names(B_tot_deriv))])

    # All Basis Functions
    spline_basis_fun_list <- list(
      remove_col_index = remove_col_index,
      B_st = B_st,
      B_st_deriv = B_st_deriv,
      B_st_pred = B_st_pred,
      B_st_deriv_pred = B_st_deriv_pred,
      B_t = B_t,
      B_t_deriv = B_t_deriv,
      B_t_pred = B_t_pred,
      B_t_pred_deriv = B_t_pred_deriv,
      B_h_deriv = B_h_deriv,
      B_deriv_g_re = B_deriv_g_re,
      B_tot_deriv = B_tot_deriv,
      B_deriv_t = B_deriv_t,
      B_deriv_st = B_deriv_st
    )
  }



  return(spline_basis_fun_list)
}

#' Creating spline basis functions
#'
#' @param x Age in years CE
#' @param xl minimum Age
#' @param xr maximum Age
#' @param nseg number of sections
#' @param deg Degree of polynomial
#' @param data Input data
#' @noRd
tpower <- function(x, t, p) {
  # Truncated p-th power function
  return((x - t)^p * (x > t))
}
bs_bbase <- function(x, xl = min(x), xr = max(x), # 30
                     #nseg = 10,
                     #nseg = 8,
                     nseg = NULL,
                     deg = 3) {
  if(is.null(nseg)){
    nseg <- round(deg / (1 + deg / length(x)))
  }
  # Construct B-spline basis
  dx <- (xr - xl) / nseg
  knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
  print(length(knots))
  P <- outer(x, knots, tpower, deg)
  n <- dim(P)[2]
  D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx^deg)
  B <- (-1)^(deg + 1) * P %*% t(D)
  print(dim(B))
  return(B)
}
# Old basis function approach
# bs_bbase <- function(x,
#                      xl = min(x),
#                      xr = max(x),
#                      deg = 3,
#                      nseg = 20){
#                      #nseg = NULL){
#   # Create basis functions------------------------------------------------------
#   if(is.null(nseg)){
#     nseg <- round(deg / (1 + deg / length(x)))
#   }
#
#   #df <- sqrt(length(x)) - 4
#   # too big
#   #nseg <- round(df/(1+df/length(x)))
#
#   # Compute the length of the partitions
#   dx <- (xr - xl) / nseg
#   # Create equally spaced knots
#   knots <- seq(xl - deg * dx,
#     xr + deg * dx,
#     by = dx
#   )
#   print(length(knots))
#   # Use bs() function to generate the B-spline basis
#   get_bs_matrix <- matrix(
#     splines::bs(x,
#       knots = knots,
#       degree = deg, Boundary.knots = c(knots[1], knots[length(knots)])
#     ),
#     nrow = length(x)
#   )
#   # Remove columns that contain zero only
#   #bs_matrix <- get_bs_matrix[, -c(1:deg, ncol(get_bs_matrix):(ncol(get_bs_matrix) - deg))]
#   bs_matrix <-get_bs_matrix
#   print(dim(bs_matrix))
#   return(bs_matrix)
# }
