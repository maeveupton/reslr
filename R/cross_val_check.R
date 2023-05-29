#' Cross validation check for spline in time, spline in space time and GAM in order to select the most appropriate number of knots when creating basis functions.
#'
#' @param raw_data Raw input data
#' @param spline_nseg This setting is focused on the Noisy Input Spline model. It provides the number of segments used to create basis functions.
#' @param spline_nseg_t This setting is focused on the Noisy Input Generalised Additive Model. It provides the number of segments used to create basis functions.
#' @param spline_nseg_st This setting is focused on the Noisy Input Generalised Additive Model. It provides the number of segments used to create basis functions.
#' @param model_type The user selects their statistical model type. The user can select a Noisy Input Spline in Time using "ni_spline_t". The user can select a Noisy Input Spline in Space Time using "ni_spline_st". The user can select a Noisy Input Generalised Additive Model using "ni_gam_decomp".
#' @param n_iterations Number of iterations. Increasing this value will increase the computational run time.
#' @param n_burnin Size of burn-in. This number removes a certain number of samples at the beginning.
#' @param n_thin Amount of thinning.
#' @param n_chains Number of MCMC chains. The number of times the model will be run.
#' @param n_fold Number of folds required in the cross validation. The default is 5 fold cross validation.
#' @param seed If the user wants reproducible results, seed stores the output when random selection was used in the creation of the 5 fold cross validation.
#'
#' @return A list containing the model comparison measures, e.g. Root Mean Square Error (RMSE)
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' cross_val_check(raw_data = data, model_type = "ni_spline_t")
cross_val_check <- function(raw_data,
                            spline_nseg = NULL,
                            spline_nseg_t = 20,
                            spline_nseg_st = 6,
                            n_iterations = 5000,
                            n_burnin = 1000,
                            n_thin = 4,
                            n_chains = 3,
                            model_type,
                            n_fold = 5,
                            seed = NULL) {
  # Cross Validation tests-----------------
  set.seed(seed)
  # Input data
  data <- raw_data
  df_split_index <- dismo::kfold(data,
                                 k = n_fold,
                                 by = data$SiteName)
  data$CV_fold <- df_split_index
  # Empty list for model runs
  model_run_list <- list()
  for (i in 1:n_fold) {
    if (model_type == "ni_gam_decomp") {
      # Segment your data by fold using the which() function
      CV_fold <- base::which(df_split_index == i, arr.ind = TRUE)
      test_set <- data[CV_fold, ] %>%
        dplyr::mutate(SiteName = as.factor(paste0(Site, ",", "\n", " ", Region)))
      training_set <- data[-CV_fold, ]
      # reslr_load
      input_train <- reslr_load(training_set,
                                cross_val = TRUE,
                                test_set = test_set
      )
      # reslr_mcmc
      train_output <- reslr_mcmc(input_train,
        model_type = model_type,
        spline_nseg_t = spline_nseg_t,
        spline_nseg_st = spline_nseg_st,
        n_iterations = n_iterations,
        n_burnin = n_burnin,
        n_thin = n_thin,
        n_chains = n_chains
      )
    } else {

      # Segment your data by fold using the which() function
      CV_fold <- base::which(df_split_index == i, arr.ind = TRUE)
      test_set <- data[CV_fold, ] %>%
        dplyr::mutate(SiteName = as.factor(paste0(Site, ",", "\n", " ", Region)))
      training_set <- data[-CV_fold, ]
      # reslr_load
      input_train <- reslr_load(training_set,
                                cross_val = TRUE,
                                test_set = test_set
      )
      # reslr_mcmc
      train_output <- reslr_mcmc(input_train,
        model_type = model_type,
        spline_nseg = spline_nseg,
        n_iterations = n_iterations,
        n_burnin = n_burnin,
        n_thin = n_thin,
        n_chains = n_chains
      )
    }
    # Check convergence of model:
    summary(train_output)
    # Take out the dataframe with true & predicted
    output_df <- train_output$output_dataframes
    # Column to identify the fold number in the loop
    output_df$CV_fold_number <- as.character(i)
    # Append this df into a list to combine to do the tests
    model_run_list[i] <- list(output_df)
  }
  browser()

  # Combining all the dataframes
  CV_model_run_df <- suppressWarnings(
    dplyr::bind_rows(model_run_list)
  )
  # Removing rows without the test set:
  CV_model_df <- CV_model_run_df %>%
    dplyr::filter(is.na(CV_fold) == FALSE)

  # Mean Error & Mean Absolute Error & Root mean square error for each fold:
  ME_MAE_RSME_fold <- CV_model_df %>%
    dplyr::mutate(CV_fold_number = as.factor(CV_fold_number)) %>%
    dplyr::group_by(CV_fold_number) %>%
    dplyr::reframe(
      RSME = unique(sqrt((sum(RSL - pred)^2) / n())),
      MAE = unique(sum(abs(RSL - pred)) / n()),
      ME = unique(mean(RSL - pred))
    )
  # Mean Error & Mean Absolute Error & Root mean square error overall
  ME_MAE_RSME_overall <- CV_model_df %>%
    dplyr::reframe(
      RSME = unique(sqrt((sum(RSL - pred)^2) / n())),
      MAE = unique(sum(abs(RSL - pred)) / n()),
      ME = unique(mean(RSL - pred))
    )

  # Mean Error & Mean Absolute Error & Root mean square error for each fold & site:
  ME_MAE_RSME_fold_site <- CV_model_df %>%
    dplyr::mutate(CV_fold_number = as.factor(CV_fold_number)) %>%
    dplyr::group_by(SiteName, CV_fold_number) %>%
    dplyr::reframe(
      RSME = unique(sqrt((sum(RSL - pred)^2) / n())),
      MAE = unique(sum(abs(RSL - pred)) / n()),
      ME = unique(mean(RSL - pred))
    )

  # Mean Error & Mean Absolute Error & Root mean square error for each site:
  ME_MAE_RSME_site <- CV_model_df %>%
    dplyr::group_by(SiteName) %>%
    dplyr::reframe(
      RSME = unique(sqrt((sum(RSL - pred)^2) / n())),
      MAE = unique(sum(abs(RSL - pred)) / n()),
      ME = unique(mean(RSL - pred))
    )


  # True vs Predicted plot
  true_pred_plot <- ggplot2::ggplot(data = CV_model_df, ggplot2::aes(x = RSL, y = pred)) +
    ggplot2::geom_point(color = "red") +
    ggplot2::geom_abline(
      data = CV_model_df,
      ggplot2::aes(intercept = 0, slope = 1, colour = "True = Predicted")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 9, face = "bold"),
      axis.text = ggplot2::element_text(size = 9),
      strip.background = ggplot2::element_rect(fill = c("white")),
      strip.text = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 7),
      legend.title = ggplot2::element_blank(),
      legend.justification = c(1, 0),
      axis.text.x = ggplot2::element_text(size = 8),
      axis.text.y = ggplot2::element_text(size = 8)
    ) +
    ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
    # legend.position = c(1, 0.5))+
    ggplot2::labs(
      x = "True Relative Sea Level (m)",
      y = "Predicted Relative Sea Level (m)"
    ) +
    ggplot2::scale_colour_manual("",
      values = c(
        "True = Predicted" = "black"
      )
    ) +
    ggplot2::facet_wrap(~SiteName, scales = "free")


  # Return a list of CV tests
  cross_validation_tests <- list(
    ME_MAE_RSME_fold_site = ME_MAE_RSME_fold_site,
    ME_MAE_RSME_site = ME_MAE_RSME_site,
    ME_MAE_RSME_overall = ME_MAE_RSME_overall,
    ME_MAE_RSME_fold = ME_MAE_RSME_fold,
    true_pred_plot = true_pred_plot
  )
  return(cross_validation_tests)
}
