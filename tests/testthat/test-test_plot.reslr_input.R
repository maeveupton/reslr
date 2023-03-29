# co <- function(expr) capture.output(expr, file = "NUL")
#
# # Testing plotting functions
# data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
# reslr_input_1 <- reslr_load(data = data)
# jags_output_1 <- reslr_mcmc(input_data = reslr_input_1,
#                             model_type = "eiv_slr_t",
#                             n_iterations = 10,
#                             n_burnin = 1,
#                             n_thin = 1,
#                             n_chains = 1)
#
# testthat::test_that("Basic reslr_input plot", {
#   p <- plot(reslr_input_1)
#   testthat::expect_type(p,"ggplot")
#   #vdiffr::expect_doppelganger("reslr_input", p)
#   # p <- plot(reslr, colour = FALSE)
#   # testthat::expect_doppelganger("simmr_input_no_col", p)
#   # p <- plot(simmr_1, tracers = c(2, 1))
#   # testthat::expect_doppelganger("simmr_input_rev_tracers", p)
# })
#
#
# #
# # # Load in with reslr_load
# # reslr_1 <-
# #   reslr_load(
# #     data = NAACproxydata,
# #     n_prediction = 100,
# #     tide_gauge_included = FALSE,
# #     input_Age_type = "CE"
# #   )
# #
# # # ??
# # testthat::test_that("basic reslr_input plot", {
# #   p <- plot(reslr_1)
# #   testthat::expect_type(p,"ggplot")
# #   #vdiffr::expect_doppelganger("reslr_input", p)
# #   # p <- plot(reslr, colour = FALSE)
# #   # testthat::expect_doppelganger("simmr_input_no_col", p)
# #   # p <- plot(simmr_1, tracers = c(2, 1))
# #   # testthat::expect_doppelganger("simmr_input_rev_tracers", p)
# # })
