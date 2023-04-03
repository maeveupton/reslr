# co <- function(expr) capture.output(expr, file = "NUL")
#
# # Testing plot result of the simple linear regression for 1 site
# testthat::test_that("Testing SLR", {
#   data("NAACproxydata")
#   data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#   reslr_input_slr<- reslr_load(data = data,
#                               n_prediction = 100,
#                               include_tide_gauge = FALSE,
#                               include_linear_rate = FALSE,
#                               list_preferred_TGs = NULL,
#                               TG_minimum_dist_proxy = FALSE,
#                               all_TG_1deg = FALSE,
#                               input_Age_type = "CE")
#   jags_output_slr <- reslr_mcmc(
#     input_data = reslr_input_slr,
#     model_type = "eiv_slr_t",
#     n_iterations = 10,
#     n_burnin = 1,
#     n_thin = 1,
#     n_chains = 1
#   )
#
#   p <- plot(jags_output_slr)
#   vdiffr::expect_doppelganger("outputplot1siteSLR", p)
# })
#
# # Testing plot the result of the 1 cp model for 1 site
# testthat::test_that("Testing cp 1", {
#   data("NAACproxydata")
#   data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#   reslr_input_cp1 <- reslr_load(data = data,
#                               n_prediction = 100,
#                               include_tide_gauge = FALSE,
#                               include_linear_rate = FALSE,
#                               list_preferred_TGs = NULL,
#                               TG_minimum_dist_proxy = FALSE,
#                               all_TG_1deg = FALSE,
#                               input_Age_type = "CE")
#   jags_output_cp1 <- reslr_mcmc(
#     input_data = reslr_input_cp1,
#     model_type = "eiv_cp_t",
#     n_iterations = 10,
#     n_burnin = 1,
#     n_thin = 1,
#     n_chains = 1,
#     n_cp = 1
#   )
#
#   p <- plot(jags_output_cp1)
#   vdiffr::expect_doppelganger("outputplot1sitecp1", p)
# })
#
# # Testing plots of the 2 cp model for 1 site
# testthat::test_that("Testing cp 2", {
#   data("NAACproxydata")
#   data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#   reslr_input_1 <- reslr_load(data = data,
#                               n_prediction = 100,
#                               include_tide_gauge = FALSE,
#                               include_linear_rate = FALSE,
#                               list_preferred_TGs = NULL,
#                               TG_minimum_dist_proxy = FALSE,
#                               all_TG_1deg = FALSE,
#                               input_Age_type = "CE")
#   jags_output_1 <- reslr_mcmc(
#     input_data = reslr_input_1,
#     model_type = "eiv_cp_t",
#     n_iterations = 10,
#     n_burnin = 1,
#     n_thin = 1,
#     n_chains = 1,
#     n_cp = 2
#   )
#
#   p <- plot(jags_output_1)
#   vdiffr::expect_doppelganger("outputplot1sitecp2", p)
# })
#
# # Testing the plots of the EIV IGP in t for 1 site
# testthat::test_that("Testing eiv igp in time", {
#   data("NAACproxydata")
#   data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#   reslr_input_1 <- reslr_load(data = data,
#                               n_prediction = 100,
#                               include_tide_gauge = FALSE,
#                               include_linear_rate = FALSE,
#                               list_preferred_TGs = NULL,
#                               TG_minimum_dist_proxy = FALSE,
#                               all_TG_1deg = FALSE,
#                               input_Age_type = "CE")
#   jags_output_1 <- reslr_mcmc(
#     input_data = reslr_input_1,
#     model_type = "eiv_igp_t",
#     n_iterations = 10,
#     n_burnin = 1,
#     n_thin = 1,
#     n_chains = 1
#   )
#
#   p <- plot(jags_output_1)
#   vdiffr::expect_doppelganger("outputplot1siteeivigp", p)
# })
#
# # Testing the plots of the NI spline in t for 1 site
# testthat::test_that("Testing ni spline in time", {
#   data("NAACproxydata")
#   data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#   reslr_input_1 <- reslr_load(data = data,
#                               n_prediction = 100,
#                               include_tide_gauge = FALSE,
#                               include_linear_rate = FALSE,
#                               list_preferred_TGs = NULL,
#                               TG_minimum_dist_proxy = FALSE,
#                               all_TG_1deg = FALSE,
#                               input_Age_type = "CE")
#   jags_output_1 <- reslr_mcmc(
#     input_data = reslr_input_1,
#     model_type = "ni_spline_t",
#     n_iterations = 10,
#     n_burnin = 1,
#     n_thin = 1,
#     n_chains = 1
#   )
#
#   p <- plot(jags_output_1)
#   vdiffr::expect_doppelganger("outputplot1sitenisplinet", p)
# })
#
# # Testing the plots of the NI spline in space time for 2 site
# testthat::test_that("Testing ni spline in space time", {
#   data("NAACproxydata")
#   data <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island","Nassau"))
#   reslr_input_1 <- reslr_load(data = data,
#                               n_prediction = 100,
#                               include_tide_gauge = FALSE,
#                               include_linear_rate = FALSE,
#                               list_preferred_TGs = NULL,
#                               TG_minimum_dist_proxy = FALSE,
#                               all_TG_1deg = FALSE,
#                               input_Age_type = "CE")
#   jags_output_1 <- reslr_mcmc(
#     input_data = reslr_input_1,
#     model_type = "ni_spline_st",
#     n_iterations = 10,
#     n_burnin = 1,
#     n_thin = 1,
#     n_chains = 1
#   )
#
#   p <- plot(jags_output_1)
#   vdiffr::expect_doppelganger("outputplot2sitesnisplinest", p)
# })
#
# # Testing the plots of the NI GAM decomposition for 4 site & closest TG & linear rates
# testthat::test_that("Testing ni GAM decomposition with closest tide gauge and linear rates", {
#   data("NAACproxydata")
#   data <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island","Nassau","Placentia","Barn Island"))
#   reslr_input_1 <- reslr_load(data = data,
#                               n_prediction = 100,
#                               include_tide_gauge = TRUE,
#                               include_linear_rate = TRUE,
#                               list_preferred_TGs = c("ARGENTIA","MAYPORT", "JACKSONVILLE","LAKE WORTH PIER",
#                                                      "MAYPORT (BAR PILOTS DOCK), FLORIDA"),
#                               TG_minimum_dist_proxy = FALSE,
#                               all_TG_1deg = FALSE,
#                               input_Age_type = "CE")
#   jags_output_1 <- reslr_mcmc(
#     input_data = reslr_input_1,
#     model_type = "ni_gam_decomp",
#     n_iterations = 10,
#     n_burnin = 1,
#     n_thin = 1,
#     n_chains = 1
#   )
#
#   p <- plot(jags_output_1)
#   vdiffr::expect_doppelganger("outputplot1sitenigamTG", p)
# })
#
# # # Testing all loading options for plotting functions for 1 site and tide gauge
# # reslr_input_2 <- reslr_load(data = data,
# #                             n_prediction = 100,
# #                             include_tide_gauge = TRUE,
# #                             TG_minimum_dist_proxy = TRUE,
# #                             include_linear_rate = TRUE,
# #                             input_Age_type = "CE")
# #
# # testthat::test_that("Basic reslr_input plot with tide gauges", {
# #   p2 <- plot(reslr_input_2,
# #              plot_tide_gauges = TRUE)
# #   vdiffr::expect_doppelganger("inputplotTG1site", p2)
# # })
# #
# #
# # # Testing plotting functions for multiple sites
# # multidata <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island","Barn Island","Nassau"))
# # reslr_input_3 <- reslr_load(data = multidata,
# #                             n_prediction = 100,
# #                             include_tide_gauge = FALSE,
# #                             include_linear_rate = FALSE,
# #                             input_Age_type = "CE")
# #
# # testthat::test_that("Basic reslr_input plot for multiple sites", {
# #   p3 <- plot(reslr_input_3)
# #   vdiffr::expect_doppelganger("inputplot3sites", p3)
# # })
# #
# # # Testing plotting functions for multiple sites and tide gauges
# # multidata <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island","Barn Island","Nassau"))
# # reslr_input_4 <- reslr_load(data = multidata,
# #                             n_prediction = 100,
# #                             include_tide_gauge = TRUE,
# #                             include_linear_rate = FALSE,
# #                             TG_minimum_dist_proxy = TRUE,
# #                             input_Age_type = "CE")
# #
# # testthat::test_that("Basic reslr_input plot for multiple sites and tide gauges", {
# #   p4 <- plot(reslr_input_4)
# #   vdiffr::expect_doppelganger("inputplot3sitesTG", p4)
# # })
