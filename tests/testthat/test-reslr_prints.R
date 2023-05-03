# Testing printing functions
data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
reslr_input_1 <- reslr_load(data = data)
# Simple Linear Regression
jags_output_1 <- reslr_mcmc(
  input_data = reslr_input_1,
  model_type = "eiv_slr_t",
  n_iterations = 10,
  n_burnin = 1,
  n_thin = 1,
  n_chains = 1
)

# Test printing input
testthat::test_that("print.reslr_input", {
  testthat::expect_output(print(reslr_input_1))
})
# Test printing output
testthat::test_that("print.reslr_output", {
  testthat::expect_output(print(jags_output_1))
})
# Test summary function
test_that("summary.reslr_output", {
  testthat::expect_output(summary(jags_output_1))
})

# Change point
jags_output_2 <- reslr_mcmc(
  input_data = reslr_input_1,
  model_type = "eiv_cp_t",
  n_iterations = 10,
  n_burnin = 1,
  n_thin = 1,
  n_chains = 1
)

# Test printing output
testthat::test_that("print.reslr_output", {
  testthat::expect_output(print(jags_output_2))
})
# Test summary function
test_that("summary.reslr_output", {
  testthat::expect_output(summary(jags_output_2))
})

# EIV IGP
jags_output_3 <- reslr_mcmc(
  input_data = reslr_input_1,
  model_type = "eiv_igp_t",
  n_iterations = 10,
  n_burnin = 1,
  n_thin = 1,
  n_chains = 1
)

# Test printing output
testthat::test_that("print.reslr_output", {
  testthat::expect_output(print(jags_output_3))
})
# Test summary function
test_that("summary.reslr_output", {
  testthat::expect_output(summary(jags_output_3))
})

# Ni spline in time
jags_output_4 <- reslr_mcmc(
  input_data = reslr_input_1,
  model_type = "ni_spline_t",
  n_iterations = 10,
  n_burnin = 1,
  n_thin = 1,
  n_chains = 1
)

# Test printing output
testthat::test_that("print.reslr_output", {
  testthat::expect_output(print(jags_output_4))
})
# Test summary function
test_that("summary.reslr_output", {
  testthat::expect_output(summary(jags_output_4))
})

# Multiple sites
data2sites <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island", "Nassau"))
reslr_input_2 <- reslr_load(data = data2sites)
# Ni spline in space time
jags_output_5 <- reslr_mcmc(
  input_data = reslr_input_2,
  model_type = "ni_spline_st",
  n_iterations = 10,
  n_burnin = 1,
  n_thin = 1,
  n_chains = 1
)
# Test printing input
testthat::test_that("print.reslr_input", {
  testthat::expect_output(print(reslr_input_2))
})
# Test printing output
testthat::test_that("print.reslr_output", {
  testthat::expect_output(print(jags_output_5))
})
# Test summary function
test_that("summary.reslr_output", {
  testthat::expect_output(summary(jags_output_5))
})

# Multiple sites & tide gauges & linear rates
data2sites <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island", "Nassau"))
reslr_input_3 <- reslr_load(
  data = data2sites,
  prediction_interval = 100,
  include_tide_gauge = TRUE,
  include_linear_rate = TRUE,
  list_preferred_TGs = NULL,
  TG_minimum_dist_proxy = TRUE,
  all_TG_1deg = FALSE,
  input_Age_type = "CE"
)
# Ni spline in space time
jags_output_6 <- reslr_mcmc(
  input_data = reslr_input_3,
  model_type = "ni_spline_st",
  n_iterations = 10,
  n_burnin = 1,
  n_thin = 1,
  n_chains = 1
)
# Test printing input
testthat::test_that("print.reslr_input", {
  testthat::expect_output(print(reslr_input_3))
})
# Test printing output
testthat::test_that("print.reslr_output", {
  testthat::expect_output(print(jags_output_6))
})
# Test summary function
test_that("summary.reslr_output", {
  testthat::expect_output(summary(jags_output_6))
})
