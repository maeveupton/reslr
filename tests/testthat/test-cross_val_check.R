# Testing the cross validation function for ni spline in time for 1 site
testthat::test_that("Testing cross validation", {
  data("NAACproxydata")
  data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
  # Cross Validation test
  cv <- cross_val_check(data = data,
                        prediction_grid_res = 100,
                        model_type ="ni_spline_t",
                        n_iterations = 10,
                        n_burnin = 1,
                        n_thin = 1,
                        n_chains = 1,
                        n_fold = 2)

  testthat::expect_true(is.list(cv))
  testthat::expect_true(is.data.frame(cv$CV_model_df))
  testthat::expect_true(is.numeric(cv$total_coverage))
})

