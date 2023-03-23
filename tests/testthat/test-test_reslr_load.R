# # #testthat::context("reslr_load")
#
# library(reslr)
#
# # Full dataset test
# testthat::test_that("simplest example", {
#   data("NAACproxydata")
#   data_testsite <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#   # Load in with reslr_load
#   reslr_1 <-
#     reslr_load(
#       data = NAACproxydata,
#       n_prediction = 100,
#       tide_gauge_included = FALSE,
#       input_Age_type = "CE"
#     )
#   testthat::expect_s3_class(reslr_1, "reslr_input")
#   testthat::expect_true(is.data.frame(reslr_1$data))
#   testthat::expect_true(is.data.frame(reslr_1$data_grid))
# })
#
# # Filtering for 1 site to increase speed
# testthat::test_that("simplest example", {
#   data("NAACproxydata")
#   data_testsite <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#   # Load in with reslr_load
#   reslr_1 <-
#     reslr_load(
#       data = data_testsite,
#       n_prediction = 100,
#       tide_gauge_included = FALSE,
#       input_Age_type = "CE"
#     )
#   testthat::expect_s3_class(reslr_1, "reslr_input")
#   testthat::expect_true(is.data.frame(reslr_1$data))
#   testthat::expect_true(is.data.frame(reslr_1$data_grid))
# })
#
# # Testing function for additional tide gauge data
# testthat::test_that("simplest example with tide gauge", {
#   data("NAACproxydata")
#   data_testsite <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#
#   # Load in with reslr_load
#   reslr_1 <-
#     reslr_load(
#       data = data_testsite,#NAACproxydata,
#       n_prediction = 100,
#       tide_gauge_included = TRUE,
#       input_Age_type = "CE"
#     )
#   testthat::expect_s3_class(reslr_1, "reslr_input")
#   testthat::expect_true(is.data.frame(reslr_1$data))
#   testthat::expect_true(is.data.frame(reslr_1$data_grid))
# })
#
# # # Testing function for additional tide gauge data for full dataset
# # Need to check this one
# # testthat::test_that("simplest example with tide gauge", {
# #   data("NAACproxydata")
# #
# #   # Load in with reslr_load
# #   reslr_1 <-
# #     reslr_load(
# #       data = NAACproxydata,
# #       n_prediction = 100,
# #       tide_gauge_included = TRUE,
# #       input_Age_type = "CE"
# #     )
# #   testthat::expect_s3_class(reslr_1, "reslr_input")
# #   testthat::expect_true(is.data.frame(reslr_1$data))
# #   testthat::expect_true(is.data.frame(reslr_1$data_grid))
# # })
