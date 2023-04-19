# Testing the simple linear regression for 1 site
testthat::test_that("Testing SLR", {
  data("NAACproxydata")
  data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
  reslr_input_1 <- reslr_load(data = data,
                              prediction_interval = 100,
                              include_tide_gauge = FALSE,
                              include_linear_rate = FALSE,
                              list_preferred_TGs = NULL,
                              TG_minimum_dist_proxy = FALSE,
                              all_TG_1deg = FALSE,
                              input_Age_type = "CE")
  jags_output_1 <- reslr_mcmc(
    input_data = reslr_input_1,
    model_type = "eiv_slr_t",
    n_iterations = 10,
    n_burnin = 1,
    n_thin = 1,
    n_chains = 1
  )

  testthat::expect_s3_class(jags_output_1, c("reslr_output", "eiv_slr_t"))
  testthat::expect_true(is.data.frame(jags_output_1$data))
  testthat::expect_true(is.data.frame(jags_output_1$data_grid))
  testthat::expect_true(is.data.frame(jags_output_1$output_dataframes))
})

# Testing the 1 cp model for 1 site
testthat::test_that("Testing cp 1", {
  data("NAACproxydata")
  data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
  reslr_input_1 <- reslr_load(data = data,
                              prediction_interval = 100,
                              include_tide_gauge = FALSE,
                              include_linear_rate = FALSE,
                              list_preferred_TGs = NULL,
                              TG_minimum_dist_proxy = FALSE,
                              all_TG_1deg = FALSE,
                              input_Age_type = "CE")
  jags_output_1 <- reslr_mcmc(
    input_data = reslr_input_1,
    model_type = "eiv_cp_t",
    n_iterations = 10,
    n_burnin = 1,
    n_thin = 1,
    n_chains = 1,
    n_cp = 1
  )

  testthat::expect_s3_class(jags_output_1, c("reslr_output", "eiv_cp1_t"))
  testthat::expect_true(is.data.frame(jags_output_1$data))
  testthat::expect_true(is.data.frame(jags_output_1$data_grid))
  testthat::expect_true(is.data.frame(jags_output_1$output_dataframes))
})

# Testing the 2 cp model for 1 site
testthat::test_that("Testing cp 2", {
  data("NAACproxydata")
  data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
  reslr_input_1 <- reslr_load(data = data,
                              prediction_interval = 100,
                              include_tide_gauge = FALSE,
                              include_linear_rate = FALSE,
                              list_preferred_TGs = NULL,
                              TG_minimum_dist_proxy = FALSE,
                              all_TG_1deg = FALSE,
                              input_Age_type = "CE")
  jags_output_1 <- reslr_mcmc(
    input_data = reslr_input_1,
    model_type = "eiv_cp_t",
    n_iterations = 10,
    n_burnin = 1,
    n_thin = 1,
    n_chains = 1,
    n_cp = 2
  )

  testthat::expect_s3_class(jags_output_1, c("reslr_output", "eiv_cp2_t"))
  testthat::expect_true(is.data.frame(jags_output_1$data))
  testthat::expect_true(is.data.frame(jags_output_1$data_grid))
  testthat::expect_true(is.data.frame(jags_output_1$output_dataframes))
})

# Testing the EIV IGP in t for 1 site
testthat::test_that("Testing eiv igp in time", {
  data("NAACproxydata")
  data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
  reslr_input_1 <- reslr_load(data = data,
                              prediction_interval = 100,
                              include_tide_gauge = FALSE,
                              include_linear_rate = FALSE,
                              list_preferred_TGs = NULL,
                              TG_minimum_dist_proxy = FALSE,
                              all_TG_1deg = FALSE,
                              input_Age_type = "CE")
  jags_output_1 <- reslr_mcmc(
    input_data = reslr_input_1,
    model_type = "eiv_igp_t",
    n_iterations = 10,
    n_burnin = 1,
    n_thin = 1,
    n_chains = 1
  )

  testthat::expect_s3_class(jags_output_1, c("reslr_output", "eiv_igp_t"))
  testthat::expect_true(is.data.frame(jags_output_1$data))
  testthat::expect_true(is.data.frame(jags_output_1$data_grid))
  testthat::expect_true(is.data.frame(jags_output_1$output_dataframes))
})

# Testing the NI spline in t for 1 site
testthat::test_that("Testing ni spline in time", {
  data("NAACproxydata")
  data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
  reslr_input_1 <- reslr_load(data = data,
                              prediction_interval = 100,
                              include_tide_gauge = FALSE,
                              include_linear_rate = FALSE,
                              list_preferred_TGs = NULL,
                              TG_minimum_dist_proxy = FALSE,
                              all_TG_1deg = FALSE,
                              input_Age_type = "CE")
  jags_output_1 <- reslr_mcmc(
    input_data = reslr_input_1,
    model_type = "ni_spline_t",
    n_iterations = 10,
    n_burnin = 1,
    n_thin = 1,
    n_chains = 1
  )

  testthat::expect_s3_class(jags_output_1, c("reslr_output", "ni_spline_t"))
  testthat::expect_true(is.data.frame(jags_output_1$data))
  testthat::expect_true(is.data.frame(jags_output_1$data_grid))
  testthat::expect_true(is.data.frame(jags_output_1$output_dataframes))
})

# Testing the NI spline in space time for 2 site
testthat::test_that("Testing ni spline in space time", {
  data("NAACproxydata")
  data <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island","Nassau"))
  reslr_input_1 <- reslr_load(data = data,
                              prediction_interval = 100,
                              include_tide_gauge = FALSE,
                              include_linear_rate = FALSE,
                              list_preferred_TGs = NULL,
                              TG_minimum_dist_proxy = FALSE,
                              all_TG_1deg = FALSE,
                              input_Age_type = "CE")
  jags_output_1 <- reslr_mcmc(
    input_data = reslr_input_1,
    model_type = "ni_spline_st",
    n_iterations = 10,
    n_burnin = 1,
    n_thin = 1,
    n_chains = 1
  )

  testthat::expect_s3_class(jags_output_1, c("reslr_output", "ni_spline_st"))
  testthat::expect_true(is.data.frame(jags_output_1$data))
  testthat::expect_true(is.data.frame(jags_output_1$data_grid))
  testthat::expect_true(is.data.frame(jags_output_1$output_dataframes))
})

# Testing the NI GAM decomposition for 4 site & closest TG & linear rates
testthat::test_that("Testing ni GAM decomposition with closest tide gauge and linear rates", {
  data("NAACproxydata")
  data <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island","Nassau","Placentia","Barn Island"))
  reslr_input_1 <- reslr_load(data = data,
                              prediction_interval = 100,
                              include_tide_gauge = TRUE,
                              include_linear_rate = TRUE,
                              # list_preferred_TGs = c("ARGENTIA","MAYPORT", "JACKSONVILLE","LAKE WORTH PIER",
                              #                                             "MAYPORT (BAR PILOTS DOCK), FLORIDA"),
                              TG_minimum_dist_proxy = TRUE,
                              all_TG_1deg = FALSE,
                              input_Age_type = "CE")
  jags_output_1 <- reslr_mcmc(
    input_data = reslr_input_1,
    model_type = "ni_gam_decomp",
    n_iterations = 10,
    n_burnin = 1,
    n_thin = 1,
    n_chains = 1
  )

  testthat::expect_s3_class(jags_output_1, c("reslr_output", "ni_gam_decomp"))
  testthat::expect_true(is.data.frame(jags_output_1$data))
  testthat::expect_true(is.data.frame(jags_output_1$data_grid))
  testthat::expect_true(is.list(jags_output_1$output_dataframes))
})
