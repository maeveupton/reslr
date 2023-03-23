# # #testthat::context("reslr_load")
#
# library(reslr)
#
# testthat::test_that("Testing SLR", {
#   data("NAACproxydata")
#   data_testsite <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#   # Load in with reslr_load
#   input_data<-
#     reslr_load(
#       data = data_testsite,
#       n_prediction = 100,
#       tide_gauge_included = FALSE,
#       input_Age_type = "CE"
#     )
#   reslr_1 <- reslr_mcmc(input_data = input_data,
#                         model_type = "eiv_slr_t")
#   testthat::expect_s3_class(reslr_1, c("reslr_output","eiv_slr_t"))
#   testthat::expect_true(is.data.frame(reslr_1$data))
#   testthat::expect_true(is.data.frame(reslr_1$data_grid))
#  })
