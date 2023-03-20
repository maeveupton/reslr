#' Noisy Input Generalised Additive Models
#'
#' @param input_data Input data from the reslr_load function
#' @param model_type Statistical Model type
#' @param n_iterations Number of iterations
#' @param n_burnin Burnin value
#' @param n_thin Thinning value
#' @param n_chains Number of chains
#' @param igp_smooth Informs prior for the smoothness (correlation) parameter if model = "igp" is chosen. Choose a value between 0 and 1. Closer to 1 will increase smoothness.
#' @param n_cp Number of change points 1,2 or 3
#'
#' @return JAGS list output
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
                       n_chains = 3) {
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
                                   n_chains = 3) {
  Age <- RSL <- Age_err <- RSL_err <- SiteName <- Longitude <- Latitude <- max_Age <- min_Age <- linear_rate <- linear_rate_err <- NULL

  # Input Data -------------
  data <- input_data$data # Add the function here
  data_grid <- input_data$data_grid

  # Simple Linear Regression ----------------
  if (model_type == "eiv_slr_t") {
    model_file <-
      "model{
  ## Data model loop
  for(j in 1:n_obs)
  {
  y[j] ~ dnorm(mu_y[j],tau[j])
  t[j] ~ dnorm(mu_t[j],t_err[j]^-2)
  mu_t[j] ~ dnorm(0,0.5^-2)
  mu_y[j] <- alpha + beta*(mu_t[j])
  tau[j] <- (y_err[j]^2 + sigma_res^2)^-1

  } # end j loop

 ## Priors
 alpha ~ dnorm(0.0,0.01)
 beta ~ dnorm(0.0,0.01)
 sigma_res ~ dt(0,4^-2,1)T(0,)

 # Predictions
 for(i in 1:n_pred) {
    mu_pred[i] = alpha + beta*t_pred[i]}

  }# end"

    # Parameters to save in JAGS-----------------
    jags_pars <- c(
      "mu_pred",
      "beta",
      "alpha",
      "sigma_res"
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
    model_run <- # suppressWarnings(
      R2jags::jags(
        data = jags_data,
        parameters.to.save = jags_pars,
        model.file = textConnection(model_file),
        n.iter = n_iterations,
        n.burnin = n_burnin,
        n.thin = n_thin,
        n.chains = n_chains
      )
    # )

    # Output from mcmc------------------------
    mu_post_pred <- model_run$BUGSoutput$sims.list$mu_pred

    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = model_run, # Watch this
      jags_data = jags_data,
      data = data,
      data_grid = data_grid
    )
    # showConnections()
    # close(model_file)
    # closeAllConnections()

    # Classing the JAGS output in NIGAM time--------------
    class(jags_output) <- c("reslr_output", "eiv_slr_t")
    message("JAGS model run finished for the EIV Simple Linear Regression")
  }

  # 1 Change Point Model-------------------
  if (model_type == "eiv_cp_t" & n_cp == 1) {
    model_file <- "
        model{

        ####CP Regression model

        ###Data Loop
        for(j in 1:n_obs){
          y[j]~dnorm(mu_y[j],tau[j])
          t[j] ~ dnorm(mu_t[j],t_err[j]^-2)
          C[j] <- 1+step(mu_t[j]-cp)
          mu_t[j] ~ dnorm(0,0.5^-2)
          mu_y[j] <- alpha + beta[C[j]]*(mu_t[j]-cp)
          tau[j] <- (y_err[j]^2 + sigma_res^2)^-1

        }

      ##Priors
      alpha[1] ~ dnorm(0.0,0.01)

      beta[1]~dnorm(0.0,0.01)
      beta[2]~dnorm(0.0,0.01)

      sigma_res ~ dt(0,4^-2,1)T(0,)

      cp ~ dunif(t_min,t_max)

      for(i in 1:n_pred){
        mu_pred[i] <-alpha + beta[Cstar[i]]*(t_pred[i]-cp)
        Cstar[i] <- 1+step(t_pred[i]-cp)
        }
      }"

    # JAGS parameters to save
    jags_pars <- c(
      "mu_pred",
      "beta",
      "alpha",
      "sigma_res",
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
      model.file = textConnection(model_file),
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains
    ))

    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = model_run, # Watch this
      jags_data = jags_data,
      data = data,
      data_grid = data_grid
    )

    # Classing the JAGS output in 1 Change Point--------------
    class(jags_output) <- c("reslr_output", "eiv_cp1_t")
    message("JAGS model run finished for the EIV 1 Change Point model")
  }


  # 2 Change Point Model----------------------
  if (model_type == "eiv_cp_t" & n_cp == 2) {
    model_file <- "

    model{
    ####CP Regression model

    ###Data Loop
      for(j in 1:n_obs){
        y[j] ~ dnorm(mu_y[j],tau[j])
        t[j] ~ dnorm(mu_t[j],t_err[j]^-2)
        A[j] <- step(mu_t[j]-cp[1])
        C[j] <- 1+step(mu_t[j]-cp[2])

        mu_t[j] ~ dnorm(0,0.5^-2)
        mu_y[j] <- alpha[C[j]] + beta[A[j] + C[j]]*(mu_t[j]-cp[C[j]])

        tau[j] <- (y_err[j]^2 + sigma_res^2)^-1

      }

    ##Priors
    alpha[1] ~ dnorm(0.0,0.01)
    alpha[2] ~ dnorm(0.0,0.01)

    beta[1]~dnorm(0.0,0.01)
    beta[2] <- (alpha[2] - alpha[1])/(cp[2]-cp[1])
    beta[3]~dnorm(0.0,0.01)

    sigma_res ~ dt(0,4^-2,1)T(0,)

    cp.temp[1] ~ dunif(t_min,t_max)
    cp.temp[2] ~ dunif(t_min,t_max)

    cp[1:2]<-sort(cp.temp)

    for(i in 1:n_pred){
      Astar[i] <- step(t_pred[i]-cp[1])
      Cstar[i] <- 1+step(t_pred[i]-cp[2])
      mu_pred[i] <- alpha[Cstar[i]] + beta[Astar[i] + Cstar[i]]*(t_pred[i]-cp[Cstar[i]])
    }


  }"
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
      "sigma_res",
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
      model.file = textConnection(model_file),
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains,
      inits = myinitial
    ))

    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = model_run, # Watch this
      jags_data = jags_data,
      data = data,
      data_grid = data_grid
    )

    # Classing the JAGS output in 2 Change Point--------------
    class(jags_output) <- c("reslr_output", "eiv_cp2_t")
    message("JAGS model run finished for the EIV 2 Change Point model")
  }

  # 3 Change Point Model----------------------
  if (model_type == "eiv_cp_t" & n_cp == 3) {
    model_file <- "

model{
####CP Regression model

  ###Data Loop
    for(j in 1:n_obs)
  {
  y[j] ~ dnorm(mu_y[j],tau[j])
  t[j] ~ dnorm(mu_t[j],t_err[j]^-2)

  A[j] <- step(mu_t[j]-cp[1])
  B[j] <- step(mu_t[j]-cp[2])
  C[j] <- 1+step(mu_t[j]-cp[3])

  mu_y[j] <- alpha[B[j] + C[j]] + beta[A[j] + B[j] + C[j]]*(mu_t[j]-cp[B[j] + C[j]])
  mu_t[j] ~ dnorm(0,0.5^-2)

  tau[j] <- (y_err[j]^2 + sigma_res^2)^-1
  }

  ##Priors
  alpha[1] ~ dnorm(0.0,0.01)
  alpha[2] ~ dnorm(0.0,0.01)
  alpha[3] ~ dnorm(0.0,0.01)

  beta[1]~dnorm(0.0,0.01)
  beta[2] <- (alpha[2] - alpha[1])/(cp[2]-cp[1])
  beta[3] <- (alpha[3] - alpha[2])/(cp[3]-cp[2])
  beta[4]~dnorm(0.0,0.01)

  sigma_res ~ dt(0,4^-2,1)T(0,)

  cp.temp[1] ~ dunif(t_min,t_max)
  cp.temp[2] ~ dunif(t_min,t_max)
  cp.temp[3] ~ dunif(t_min,t_max)

  cp[1:3]<-sort(cp.temp)

    for(i in 1:n_pred){
      Astar[i] <- step(t_pred[i]-cp[1])
      Bstar[i] <- step(t_pred[i]-cp[2])
      Cstar[i] <- 1+step(t_pred[i]-cp[3])
      mu_pred[i] <- alpha[Bstar[i] + Cstar[i]] + beta[Astar[i] + Bstar[i] + Cstar[i]]*(t_pred[i]-cp[Bstar[i] + Cstar[i]])
    }


  }"
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
      "sigma_res",
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
      model.file = textConnection(model_file),
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains,
      inits = myinitial
    ))

    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = model_run, # Watch this
      jags_data = jags_data,
      data = data,
      data_grid = data_grid
    )

    # Classing the JAGS output in 3 Change Point--------------
    class(jags_output) <- c("reslr_output", "eiv_cp3_t")
    message("JAGS model run finished for the EIV 3 Change Point model")
  }

  # Errors-in-Variables Integrated Gaussian Process------------------
  if (model_type == "eiv_igp_t") {
    model_file <-
      "model{

        for(i in 1:n_obs){
          y[i]~dnorm(alpha + w.tilde.m[i],tau[i])
          t[i] ~ dnorm(mu_t[i],t_err[i]^-2)
          mu_t[i] ~ dnorm(0,0.5^-2)
          noisy_terr[i] <- sqrt((beta^2)*(t_err[i]^2))
          tau[i] <- (y_err[i]^2 + sigma_res^2 + noisy_terr[i]^2)^-1
        }

        ###Derivative process
        w.m~dmnorm(mu.w,K.inv)
        K.inv <- inverse((sigma_g^2)*K)
        K.w.inv<-inverse(K)

        for(i in 1:Ngrid){
          mu.w[i] <- beta
          K[i,i]<-1+0.00001
        ######Exponential covariance for the derivative process
          for(j in (i+1):Ngrid){
              K[i,j]<-(pow(phi,pow(Dist[i,j],1.99)))
              K[j,i]<-K[i,j]
            }
          }
        ###Expected value of the integrated Gaussian Process
        for(i in 1:Ngrid) {
          for(j in 1:n_obs) {
            K.gw[j,i]<-sum(pow(phi,quad1[j,i,])*quad2[j,i,])  #### Quadrature function
          } #End j loop
        } #End i loop

        w.tilde.m<-K.gw %*% K.w.inv %*% w.m
      # Priors
      sigma_g ~ dt(0,2^-2,1)T(0,)
      phi ~ dbeta(al,10)
      sigma_res ~ dt(0,2^-2,1)T(0,)
      alpha ~ dnorm(0,2^-2)
      beta ~ dnorm(0,2^-2)
  }##End model
  "
    # JAGS parameters to save
    jags_pars <- c(
      "phi",
      "sigma_g",
      "sigma_res",
      "w.m",
      "alpha",
      "beta"
    )

    # JAGS data
    igp_dat_list <- igp_data(data)
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
      model.file = textConnection(model_file),
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains
    ))


    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = model_run, # Watch this
      jags_data = jags_data,
      data = data,
      data_grid = data_grid
    )

    # Classing the JAGS output for eiv_igp_t--------------
    class(jags_output) <- c("reslr_output", "eiv_igp_t")
    message("JAGS model run finished for the eiv_igp_t")
  }



  # # Noisy Input GAM in Time-----------------------------------
  # if (model_type == "ni_spline_t") {
  #   # No Noise
  #   model_file <-
  #     "model {
  #
  #       for (i in 1:n_obs) {
  #         # Main likelihood
  #         y[i] ~ dnorm(mu_y[i], sigmasq_all[i]^-1)
  #         sigmasq_all[i] <- sigma_res^2 + y_err[i]^2
  #
  #         # Mean structure
  #         mu_y[i] <- r[i]
  #         }
  #
  #     # # Regional term
  #     # r <- B_t %*% b_t
  #     # for (j in 1:n_knots_t) {
  #     #   b_t[j] ~ dnorm(0, tau_t)
  #     #   #b_t[j] ~ dnorm(0, sigma_t^-2)
  #     # }
  #
  #     # Spline in time penalised
  #     r <- B_t %*% b_t
  #     b_t[1] ~ dnorm(0,1)
  #     for (j in 2:n_knots_t) {
  #       b_t[j] ~ dnorm(b_t[j-1], tau_t)
  #     }
  #
  #     # Priors
  #     # tau_t ~ dgamma(1,10^-5)# Lang paper
  #     tau_t ~ dgamma(1,1)
  #
  #     sigma_t<- tau_t^-0.5
  #     #sigma_t~ dt(0, 1^-2, 1)T(0,)
  #     sigma_res ~ dt(0, 1^-2, 1)T(0,)
  #
  #     }"
  #
  #
  #   # Parameters to save in JAGs-----------------
  #   jags_pars <- c(
  #     "mu_y",
  #     "sigma_res",
  #     "b_t",
  #     "r",
  #     "sigma_t",
  #     "sigmasq_all"
  #   )
  #   # Basis functions in time -----------------------------
  #   spline_basis_fun_list <- spline_basis_fun(
  #     data = data,
  #     data_grid = data_grid,
  #     model_type = model_type
  #   )
  #
  #   # JAGS data----------------------
  #   jags_data <- list(
  #     y = data$RSL,
  #     y_err = data$RSL_err,
  #     t = data$Age,
  #     n_obs = nrow(data),
  #     B_t = spline_basis_fun_list$B_t,
  #     n_knots_t = ncol(spline_basis_fun_list$B_t)
  #   )
  #
  #
  #
  #   # Run JAGS------------------------
  #   model_run <- suppressWarnings(R2jags::jags(
  #     data = jags_data,
  #     parameters.to.save = jags_pars,
  #     model.file = textConnection(model_file),
  #     n.iter = n_iterations,
  #     n.burnin = n_burnin,
  #     n.thin = n_thin,
  #     n.chains = n_chains
  #   ))
  #
  #   # Adding Noisy Input-------------------
  #   data <- add_noisy_input(
  #     data = data,
  #     model_run = model_run,
  #     model_type = model_type
  #   )
  #
  #   # Include Noise-----------------------
  #   noise_model_file <-
  #     "model {
  #
  #       for (i in 1:n_obs) {
  #         # Main likelihood
  #         y[i] ~ dnorm(mu_y[i], sigmasq_all[i]^-1)
  #         sigmasq_all[i] <- sigma_res^2 + y_err[i]^2 + NI_var_term[i]^2
  #
  #         # Mean structure
  #         mu_y[i] <- r[i]
  #       }
  #
  #     # Spline in Time
  #     # r <- B_t %*% b_t
  #     # for (j in 1:n_knots_t) {
  #     #    #b_t[j] ~ dnorm(0, sigma_t^-2)
  #     #    b_t[j] ~ dnorm(0, tau_t)
  #     #  }
  #
  #     # # Spline in time penalised
  #     r <- B_t %*% b_t
  #     b_t[1] ~ dnorm(0,1)
  #     for (j in 2:n_knots_t) {
  #       b_t[j] ~ dnorm(b_t[j-1], tau_t)
  #     }
  #
  #
  #
  #     # Predictions
  #     r_pred <- B_t_pred %*% b_t
  #     for (i in 1:n_pred){
  #         mu_pred[i] <- r_pred[i]
  #     }
  #
  #     # Derivatives
  #     r_deriv <- B_t_deriv %*% b_t
  #     for (i in 1:n_obs){
  #       mu_deriv[i] <- r_deriv[i]
  #     }
  #     r_pred_deriv <- B_t_pred_deriv %*% b_t
  #     for (i in 1:n_pred){
  #       mu_pred_deriv[i] <- r_pred_deriv[i]
  #     }
  #
  #
  #     # Priors
  #     # tau_t ~ dgamma(1,10^-5)# Lang paper
  #     tau_t ~ dgamma(1,1)
  #
  #     sigma_t <- tau_t^-0.5
  #     #sigma_t~ dt(0, 1^-2, 1)T(0,)
  #
  #     sigma_res ~ dt(0, 1^-2, 1)T(0,)
  #   }"
  #
  #
  #   # Parameters to save in JAGs-----------------
  #   jags_pars <- c(
  #     "mu_y",
  #     "mu_deriv",
  #     "mu_pred",
  #     "mu_pred_deriv",
  #     "r_pred_deriv",
  #     "sigma_res",
  #     "b_t",
  #     "r",
  #     "r_deriv",
  #     "sigma_t",
  #     "sigmasq_all",
  #     "r_pred"
  #   )
  #
  #   # JAGS data for second model run-----------
  #   jags_data <- list(
  #     NI_var_term = data$NI_var_term,
  #     y = data$RSL,
  #     y_err = data$RSL_err,
  #     t = data$Age,
  #     n_obs = nrow(data),
  #     t_pred = data_grid$Age,
  #     n_pred = length(data_grid$Age),
  #     B_t = spline_basis_fun_list$B_t,
  #     B_t_deriv = spline_basis_fun_list$B_t_deriv,
  #     B_t_pred = spline_basis_fun_list$B_t_pred,
  #     n_knots_t = ncol(spline_basis_fun_list$B_t),
  #     B_t_pred_deriv = spline_basis_fun_list$B_t_pred_deriv
  #   )
  #
  #
  #   # Run JAGS--------------
  #   noisy_model_run_output <-
  #     suppressWarnings(R2jags::jags(
  #       data = jags_data,
  #       parameters.to.save = jags_pars,
  #       model.file = textConnection(noise_model_file),
  #       n.iter = n_iterations,
  #       n.burnin = n_burnin,
  #       n.thin = n_thin,
  #       n.chains = n_chains
  #     ))
  #   #closeAllConnections()
  #
  #   # Output with everything-------------
  #   jags_output <- list(
  #     noisy_model_run_output = noisy_model_run_output,
  #     jags_data = jags_data,
  #     data = data,
  #     data_grid = data_grid
  #   )
  #
  #
  #   # Classing the JAGS output in NIGAM time--------------
  #   class(jags_output) <- c("reslr_output", "ni_spline_t")
  #
  #   message("JAGS model run finished for the NIGAM in time")
  # }

  # TESTING new jags for Noisy Input GAM in Time-----------------------------------
  if (model_type == "ni_spline_t") {
    # No Noise
    model_file <-
      "model {

        for (i in 1:n_obs) {
          # Main likelihood
          y[i] ~ dnorm(mu_y[i],tau) #sigmasq_all[i]^-1)
          #sigmasq_all[i] <- (sigma_res^2 + y_err[i]^2)
          #tau[i] <- (sigma_res^2 + y_err[i]^2)

          # Mean structure
          mu_y[i] <- r[i]
          }

      # Spline in time penalised
      r <- B_t %*% b_t
      b_t[1] ~ dnorm(0,1)
      for (j in 2:n_knots_t) {
        b_t[j] ~ dnorm(b_t[j-1], tau_t)
      }

      # Predictions
      r_pred <- B_t_pred %*% b_t
      for (i in 1:n_pred){
        mu_pred[i] <- r_pred[i]
      }

      # Derivatives
      r_deriv <- B_t_deriv %*% b_t
      for (i in 1:n_obs){
        mu_deriv[i] <- r_deriv[i]
      }
      r_pred_deriv <- B_t_pred_deriv %*% b_t
      for (i in 1:n_pred){
        mu_pred_deriv[i] <- r_pred_deriv[i]
      }


      # Priors
      #tau ~ dgamma(a_tau,d_tau)
      tau~ dgamma(1,1)
      tau_t ~dgamma(0.5*nu,0.5*delta*nu)
      #delta ~ dgamma(a_delta,d_delta)
      delta ~ dgamma(0.0001,0.0001)
      #tau_t ~ dgamma(1,1)

      sigma_t<- tau_t^-0.5
      #sigma_t~ dt(0, 1^-2, 1)T(0,)
      #sigma_res ~ dt(0, 1^-2, 1)T(0,)

      }"




    # Parameters to save in JAGs-----------------
    # jags_pars <- c(
    #   "mu_y",
    #   #"tau",
    #   "tau_t",
    #   #"sigma_res",
    #   "b_t",
    #   "r",
    #   "sigma_t",
    #   "sigmasq_all"
    # )
    # Basis functions in time -----------------------------
    spline_basis_fun_list <- spline_basis_fun(
      data = data,
      data_grid = data_grid,
      model_type = model_type
    )


    # Parameters to save in JAGs-----------------
    jags_pars <- c(
      "mu_y",
      "mu_deriv",
      "mu_pred",
      "mu_pred_deriv",
      "r_pred_deriv",
      # "sigma_res",
      "b_t",
      "r",
      "r_deriv",
      "sigma_t",
      # "sigmasq_all",
      "r_pred",
      "tau",
      "tau_t"
    )

    # JAGS data for second model run-----------
    jags_data <- list(
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
      B_t_pred_deriv = spline_basis_fun_list$B_t_pred_deriv,
      nu = 2
    )



    # Run JAGS------------------------
    model_run <- suppressWarnings(R2jags::jags(
      data = jags_data,
      parameters.to.save = jags_pars,
      model.file = textConnection(model_file),
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains
    ))


    # # Run JAGS--------------
    #   noisy_model_run_output <-
    #     suppressWarnings(R2jags::jags(
    #       data = jags_data,
    #       parameters.to.save = jags_pars,
    #       model.file = textConnection(noise_model_file),
    #       n.iter = n_iterations,
    #       n.burnin = n_burnin,
    #       n.thin = n_thin,
    #       n.chains = n_chains
    #     ))

    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = model_run,
      jags_data = jags_data,
      data = data,
      data_grid = data_grid
    )


    # Classing the JAGS output in NIGAM time--------------
    class(jags_output) <- c("reslr_output", "ni_spline_t")

    message("JAGS model run finished for the NIGAM in time")
  }

  # Noisy Input GAM in Space Time-------------------------------------------
  if (model_type == "ni_spline_st") {
    # No Noise
    model_file <-
      "model {

  for (i in 1:n_obs) {
   # Main likelihood
    y[i] ~ dnorm(mu_y[i], sigmasq_all[i]^-1)
    sigmasq_all[i] <- sigma_res^2 + y_err[i]^2

    # Mean structure
    mu_y[i] <- l[i]
  }

  # Non-linear local component
  # l <- B_st %*% b_st
  # for (j in 1:n_knots_st) {
  #    b_st[j] ~ dnorm(0, tau_st)
  # }

  # Non-linear local component
   l <- B_st %*% b_st
   b_st[1] ~ dnorm(0,0.1)
   for (j in 2:n_knots_st) {
      #b_st[j] ~ dnorm(b_st[1], sigma_st^-2)
      b_st[j] ~ dnorm(b_st[1], tau_st)
   }


  # Priors
  tau_st ~ dgamma(1,1)#10^-5)# Lang paper
  sigma_st <- tau_st^-0.5
  sigma_res ~ dt(0, 1^-2, 1)T(0,)


  }
  "
    # Basis functions in space time -----------------------------
    spline_basis_fun_list <- spline_basis_fun(
      data = data,
      data_grid = data_grid,
      model_type = model_type
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
      "sigma_res",
      "b_st",
      "l",
      "sigma_st",
      "sigmasq_all"
    )

    # Run JAGS------------------------
    model_run <- suppressWarnings(R2jags::jags(
      data = jags_data,
      parameters.to.save = jags_pars,
      model.file = textConnection(model_file),
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains
    ))

    # Adding Noisy Input-------------------
    data <- add_noisy_input(
      data = data,
      model_run = model_run,
      model_type = model_type
    )

    #----NI JAGS model-----
    noise_model_file <-
      "model {

        for (i in 1:n_obs) {
          # Main likelihood
          y[i] ~ dnorm(mu_y[i], sigmasq_all[i]^-1)
          sigmasq_all[i] <- sigma_res^2 + y_err[i]^2 + NI_var_term[i]^2

          # Mean structure
          mu_y[i] <- l[i]
        }

      # # Non-linear local component
      # l <- B_st %*% b_st
      # for (j in 1:n_knots_st) {
      #   b_st[j] ~ dnorm(0, sigma_st^-2)
      # }

      # Non-linear local component
      l <- B_st %*% b_st
      b_st[1] ~ dnorm(0,0.1)
      for (j in 2:n_knots_st) {
        #b_st[j] ~ dnorm(b_st[j-1], sigma_st^-2)
        b_st[j] ~ dnorm(b_st[j-1], tau_st)
      }

      # Predictions
      l_pred <- B_st_pred %*% b_st
      for (i in 1:n_pred){
        mu_pred[i] <- l_pred[i]
      }

      # Derivatives
      l_deriv <- B_st_deriv %*% b_st
      for (i in 1:n_obs){
        mu_deriv[i] <- l_deriv[i]
      }

      l_pred_deriv <- B_st_deriv_pred %*% b_st
      for (i in 1:n_pred){
        mu_pred_deriv[i] <- l_pred_deriv[i]
      }

      # Priors
        tau_st ~ dgamma(1,1)#10^-5)# Lang paper:precision too low
        sigma_st <- tau_st^-0.5
        sigma_res ~ dt(0, 1^-2, 1)T(0,)
    }"

    # JAGS input data
    jags_data <- list(
      y = data$RSL,
      NI_var_term = data$NI_var_term,
      y_err = data$RSL_err,
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
      "sigma_res",
      "b_st",
      "l",
      "l_pred",
      "sigma_st",
      "sigmasq_all"
    )

    # Run JAGS--------------
    noisy_model_run_output <-
      suppressWarnings(R2jags::jags(
        data = jags_data,
        parameters.to.save = jags_pars,
        model.file = textConnection(noise_model_file),
        n.iter = n_iterations,
        n.burnin = n_burnin,
        n.thin = n_thin,
        n.chains = n_chains
      ))
    # closeAllConnections()

    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = noisy_model_run_output,
      jags_data = jags_data,
      data = data,
      data_grid = data_grid
    )
    # Classing the JAGS output in NIGAM space time--------------
    class(jags_output) <- c("reslr_output", "ni_spline_st")
    message("JAGS model run finished for the NIGAM in space time")
  }

  # Noisy Input GAM for decomposition of RSL signal-------------------------------------------
  if (model_type == "ni_gam_decomp") {
    # No Noise
    model_file <-
      "model {

    for (i in 1:n_obs) {
    # Main likelihood
      y[i] ~ dnorm(mu_y[i], sigmasq_all[i]^-1)
      sigmasq_all[i] <- sigma_res^2 + y_err[i]^2

    # Mean structure
      mu_y[i] <- r[i] + g_z_x[i] + h_z_x[i]

    # Site specific vertical offset
      h_z_x[i] <- intercept[site[i]]

    # Linear Local component
      g_z_x[i] <- t[i] * b_g[site[i]]

    # Linear Local component + Site specific vertical offset
      g_h_z_x[i] <- h_z_x[i] + g_z_x[i]
    }

    # Regional term
    r <- B_t %*% b_t
    for (j in 1:n_knots_t) {
      b_t[j] ~ dnorm(0, sigma_t^-2)
    }

    # Prior on intercept
    for (j in 1:n_sites) {
      intercept[j] ~ dnorm(0, sigma_h^-2)
    }

    # Linear Local component
    for (j in 1:n_sites) {
      b_g[j] ~ dnorm(linear_rate[j], linear_rate_err[j]^-2)
    }

    # Priors
    sigma_t~ dt(0, 1^-2, 1)T(0,)
    sigma_h ~ dt(2.5, 2^-2, 1)T(0,)
    sigma_res ~ dt(0, 1^-2, 1)T(0,)

  }"
    # Basis functions in space time -----------------------------
    spline_basis_fun_list <- spline_basis_fun(
      data = data,
      data_grid = data_grid,
      model_type = model_type
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
      "sigma_res",
      "b_t",
      "h_z_x",
      "g_h_z_x",
      "g_z_x",
      "r",
      "intercept",
      "sigma_t",
      "b_g",
      "sigma_h",
      "sigmasq_all"
    )

    # Run JAGS------------------------
    model_run <- suppressWarnings(R2jags::jags(
      data = jags_data,
      parameters.to.save = jags_pars,
      model.file = textConnection(model_file),
      n.iter = n_iterations,
      n.burnin = n_burnin,
      n.thin = n_thin,
      n.chains = n_chains
    ))

    # Adding Noisy Input-------------------
    data <- add_noisy_input(
      data = data,
      model_run = model_run,
      model_type = model_type
    )

    #----NI JAGS model-----
    noise_model_file <-
      "model {

        for (i in 1:n_obs) {
          # Main likelihood
          y[i] ~ dnorm(mu_y[i], sigmasq_all[i]^-1)
          sigmasq_all[i] <- sigma_res^2 + y_err[i]^2 + NI_var_term[i]^2

          # Mean structure
          mu_y[i] <- r[i] + g_z_x[i] + h_z_x[i] + l[i]

          # Derivative for the Total component
          mu_deriv[i] <- r_deriv[i] + l_deriv[i] + g_z_x_deriv[i]

          # Site specific vertical offset
          h_z_x[i] <- intercept[site[i]]

          # Linear Local component
          g_z_x[i] <- t[i] * b_g[site[i]]

          # Linear Local component + Site specific vertical offset
          g_h_z_x[i] <- h_z_x[i] + g_z_x[i]

          # Linear Local component
          g_z_x_deriv[i] <- b_g[site[i]]

        }

      # Informed Regional component
      r <- B_t %*% b_t
      for (j in 1:n_knots_t) {
        b_t[j] ~ dnorm(b_t_value[j], b_t_sd_value[j]^-2)
      }

      # Derivative for Regional Term
      r_deriv <- B_t_deriv %*% b_t

      # Prediction for Regional term
      r_pred <- B_t_pred %*% b_t


      # Derivative for Predicted Regional Term
      r_pred_deriv <- B_t_pred_deriv %*% b_t


      # Non-linear local component
      l <- B_st %*% b_st
      for (j in 1:n_knots_st) {
        b_st[j] ~ dnorm(0, sigma_st^-2)
      }
      # Derivative of Non-linear local
      l_deriv <- B_st_deriv %*% b_st

      # Non-linear local component on prediction grid
      l_pred <- B_st_pred %*% b_st

      # Derivative of Non-linear local on prediction grid
      l_pred_deriv <- B_st_deriv_pred %*% b_st

      # Prior on intercept
      for (j in 1:n_sites) {
        intercept[j] ~ dnorm(h_value[j], h_sd_value[j]^-2)
      }
      # # Prior on intercept for prediction grid?
      # for (j in 1:n_sites_pred) {
      #   intercept[j] ~ dnorm(h_value[j], h_sd_value[j]^-2)
      # }

      # Linear Local component
      for (j in 1:n_sites) {
        b_g[j] ~ dnorm(linear_rate[j], linear_rate_err[j]^-2)
      }

      # # Linear Local component on Prediction grid
      # for (j in 1:n_sites_pred) {
      #   b_g_pred[j] ~ dnorm(linear_rate_pred[j], linear_rate_err_pred[j]^-2)
      # }

    # Total for the prediction grid
    for(i in 1: n_pred){
        # Site specific vertical offset
        h_z_x_pred[i] <- intercept[site_pred[i]]

        # Linear Local component
        g_z_x_pred[i] <- t_pred[i] * b_g[site_pred[i]]
        # Linear Local component Derivative
        g_z_x_pred_deriv[i] <- b_g[site_pred[i]]

        # Linear Local component + Site specific vertical offset
        g_h_z_x_pred[i] <- h_z_x_pred[i] + g_z_x_pred[i]

        # Mean structure
        mu_pred[i] <- r_pred[i] + l_pred[i] + g_z_x_pred[i] + h_z_x_pred[i]

        # Derivative for the Total component
        mu_pred_deriv[i] <- r_pred_deriv[i] + l_pred_deriv[i] + g_z_x_pred_deriv[i]

    }

    # Priors
    sigma_st ~ dt(0, 1^-2, 1)T(0,)
    sigma_res ~ dt(0, 1^-2, 1)T(0,)

  }"
    # JAGS input data
    jags_data <- list(
      NI_var_term = data$NI_var_term,
      b_t_value = model_run$BUGSoutput$median$b_t,
      b_t_sd_value = model_run$BUGSoutput$sd$b_t,
      h_value = model_run$BUGSoutput$median$intercept,
      h_sd_value = model_run$BUGSoutput$sd$intercept,
      y = data$RSL,
      y_err = data$RSL_err,
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
      "sigma_res",
      "sigma_h", # ?
      "sigma_t", # ?
      "sigma_st",
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
      "l_pred_deriv"
    )

    # Run JAGS--------------
    noisy_model_run_output <-
      suppressWarnings(R2jags::jags(
        data = jags_data,
        parameters.to.save = jags_pars,
        model.file = textConnection(noise_model_file),
        n.iter = n_iterations,
        n.burnin = n_burnin,
        n.thin = n_thin,
        n.chains = n_chains
      ))
    # closeAllConnections()

    # Output with everything-------------
    jags_output <- list(
      noisy_model_run_output = noisy_model_run_output,
      jags_data = jags_data,
      data = data,
      data_grid = data_grid
    )

    # Classing the JAGS output in NIGAM for RSL decomposition--------------
    class(jags_output) <- c("reslr_output", "ni_gam_decomp")
    message("JAGS model run finished for the NI GAM Decomposition")
  }
  return(jags_output)
}
