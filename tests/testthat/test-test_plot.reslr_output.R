#co <- function(expr) capture.output(expr, file = "NUL")

# Testing all loading options for plotting functions for 1 site
data("NAACproxydata")
data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
reslr_input_1 <- reslr_load(
  data = data,
  prediction_grid_res = 100,
  include_tide_gauge = FALSE,
  TG_minimum_dist_proxy = FALSE,
  input_age_type = "CE"
)


testthat::test_that("Basic reslr_output plot for SLR", {
  # Testing EIV SLR
  jags_output_slr <- reslr_mcmc(
    input_data = reslr_input_1,
    model_type = "eiv_slr_t",
    n_iterations = 10,
    n_burnin = 1,
    n_thin = 1,
    n_chains = 1
  )
  p1 <- plot(jags_output_slr)
  testthat::expect_true(is.list(p1))
})



testthat::test_that("Basic reslr_output plot for EIV cp 1", {
  # Testing EIV CP 1
  jags_output_cp1 <- reslr_mcmc(
    input_data = reslr_input_1,
    model_type = "eiv_cp_t",
    n_iterations = 10,
    n_burnin = 1,
    n_thin = 1,
    n_chains = 1
  )
  p2 <- plot(jags_output_cp1)
  testthat::expect_true(is.list(p2))
})


testthat::test_that("Basic reslr_output plot for EIV cp 2", {
  # Testing EIV CP 2
  jags_output_cp2 <- reslr_mcmc(
    input_data = reslr_input_1,
    model_type = "eiv_cp_t",
    n_cp = 2,
    n_iterations = 10,
    n_burnin = 1,
    n_thin = 1,
    n_chains = 1
  )
  p3 <- plot(jags_output_cp2)
  testthat::expect_true(is.list(p3))
})


testthat::test_that("Basic reslr_output plot for EIV IGP", {
  # Testing EIV IGP
  jags_output_igp <- reslr_mcmc(
    input_data = reslr_input_1,
    model_type = "eiv_igp_t",
    n_iterations = 10,
    n_burnin = 1,
    n_thin = 1,
    n_chains = 1
  )
  p4 <- plot(jags_output_igp)
  testthat::expect_true(is.list(p4))
})

testthat::test_that("Basic reslr_output plot for NI spline in t", {
  # Testing NI spline t
  jags_output_nisplinet <- reslr_mcmc(
    input_data = reslr_input_1,
    model_type = "ni_spline_t",
    n_iterations = 10,
    n_burnin = 1,
    n_thin = 1,
    n_chains = 1
  )
  p5 <- plot(jags_output_nisplinet)
  testthat::expect_true(is.list(p5))
})


# # Testing plotting functions for multiple sites
# testthat::test_that("Basic plot for multiple sites", {
#   multidata <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island", "Barn Island", "Nassau"))
#   reslr_input_3 <- reslr_load(
#     data = multidata,
#     prediction_grid_res = 100,
#     include_tide_gauge = FALSE,
#     include_linear_rate = FALSE,
#     input_age_type = "CE"
#   )
#   # Testing NI spline st
#   jags_output_nisplinest <- reslr_mcmc(
#     input_data = reslr_input_3,
#     model_type = "ni_spline_st",
#     n_iterations = 10,
#     n_burnin = 1,
#     n_thin = 1,
#     n_chains = 1
#   )
#   p6 <- plot(jags_output_nisplinest)
#   testthat::expect_true(is.list(p6))
# })

# # Testing plotting functions for multiple sites and tide gauges
# testthat::test_that("Basic plot for multiple sites and tide gauges", {
#   multidata <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island", "Barn Island", "Nassau"))
#   reslr_input_4 <- reslr_load(
#     data = multidata,
#     prediction_grid_res = 100,
#     include_tide_gauge = TRUE,
#     include_linear_rate = FALSE,
#     TG_minimum_dist_proxy = TRUE,
#     input_age_type = "CE"
#   )
#   # Testing NI gam
#   jags_output_nisplinest <- reslr_mcmc(
#     input_data = reslr_input_4,
#     model_type = "ni_gam_decomp",
#     n_iterations = 10,
#     n_burnin = 1,
#     n_thin = 1,
#     n_chains = 1
#   )
#   p7 <- plot(jags_output_nisplinest)
#   testthat::expect_true(is.list(p7))
# })
