#' Cross validation check using 5 folds for spline in time, spline in space time and GAM in order to choose the number of knots
#'
#' @param input_data Input data from the \code{reslr_load} function
#' @param spline_nseg This setting is focused on the Noisy Input Spline model. It provides the number of segments used to create basis functions.
#' @param spline_nseg_t This setting is focused on the Noisy Input Generalised Additive Model. It provides the number of segments used to create basis functions.
#' @param spline_nseg_st This setting is focused on the Noisy Input Generalised Additive Model. It provides the number of segments used to create basis functions.
#' @param model_type The user selects their statistical model type. The user can select a Noisy Input Spline in Time using "ni_spline_t". The user can select a Noisy Input Spline in Space Time using "ni_spline_st". The user can select a Noisy Input Generalised Additive Model using "ni_gam_decomp".
#' @param n_iterations Number of iterations. Increasing this value will increase the computational run time.
#' @param n_burnin Size of burn-in. This number removes a certain number of samples at the beginning.
#' @param n_thin Amount of thinning.
#' @param n_chains Number of MCMC chains. The number of times the model will be run.

#' @return A list containing the model comparison measures, e.g. Root Mean Square Error (RMSE)
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' input_data <- reslr_load(data = data)
#' cross_val_check(input_data = input_data, model_type = "ni_spline_t")
cross_val_check <- function(input_data,
                            spline_nseg = NULL,
                            spline_nseg_t = 20,
                            spline_nseg_st = 6,
                            n_iterations = 5000,
                            n_burnin = 1000,
                            n_thin = 4,
                            n_chains = 3,
                            model_type){

  # NI Spline in time ----------------
  if (model_type == "ni_spline_t") {
    # Input data
    data <- input_data$data
    data_grid <- input_data$data_grid
    # need to loop over and set a seed and the kfold will change each time
    # run_num <- i
    # cat(run_num)
    #set.seed(4867 + (run_num)^2)
    df_split_index <- dismo::kfold(data, k = 5, by = data$SiteName)
    data$CV_fold <- df_split_index

    for(i in 1:5){
      #Segement your data by fold using the which() function
      CV_fold <- base::which(df_split_index==i,arr.ind=TRUE)
      test_set <- data[CV_fold, ]
      training_set <- data[-CV_fold, ]

      test_grid_set <- data_grid[CV_fold, ]
      training_grid_set <- data_grid[-CV_fold, ]

      input_test_data <- list(test_set=test_set,
                              test_grid_set=test_grid_set)
      #class(input_test_data) <- "reslr_input"
      input_training_data <- list(training_set=training_set,
                                  training_grid_set=training_grid_set)
      #class(input_training_data) <- "reslr_input"

    # # Running for test data:
    # output_test <- reslr_mcmc(input_data=input_test_data,
    #                           model_type = "ni_spline_t",
    #                           xl = min(input_training_data$data$Age),
    #                           xr = max(input_training_data$data$Age),
    #                           spline_nseg = spline_nseg,
    #                           n_iterations = n_iterations,
    #                           n_burnin = n_burnin,
    #                           n_thin = n_thin,
    #                           n_chains = n_chains
    #
    # )
    #
    # # Running for training data:
    # output_test <- reslr_mcmc(input_training_data,
    #                           model_type = "ni_spline_t",
    #                           xl = min(input_training_data$data$Age),
    #                           xr = max(input_training_data$data$Age),
    #                           spline_nseg = spline_nseg,
    #                           n_iterations = n_iterations,
    #                           n_burnin = n_burnin,
    #                           n_thin = n_thin,
    #                           n_chains = n_chains
    # )

    }
    #RMSE <-
  }
}
