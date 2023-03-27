# Testing printing functions
data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
reslr_input_1 <- reslr_load(data = data)
jags_output_1 <- reslr_mcmc(input_data = reslr_input_1,
                            model_type = "eiv_slr_t",
                            n_iterations = 10,
                            n_burnin = 1,
                            n_thin = 1,
                            n_chains = 1)

# Test printing input
test_that("print.reslr_input", {
  testthat::expect_output(print(reslr_input_1))
})
# Test printing output
test_that("print.reslr_output", {
  testthat::expect_output(print(jags_output_1))
})
# # Test summary function --> not working
# test_that("summary.reslr_output", {
#   testthat::expect_output(summary(jags_output_1))
# })
