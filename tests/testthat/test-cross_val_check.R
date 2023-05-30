# # Testing the cross validation function for ni spline in time for 1 site
# testthat::test_that("Testing cross validation", {
#   data("NAACproxydata")
#   data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#   # Cross Validation test
#   cv <- cross_val_check(raw_data = data,
#                         model_type ="ni_spline_t",
#                         n_iterations = 1,
#                         n_burnin = 1,
#                         n_thin = 1,
#                         n_chains = 1,
#                         n_fold = 2)
#
#
#   testthat::expect_true(is.list(cv))
# })
#
# # Testing the cross validation function for ni spline in space time for 2 site
# testthat::test_that("Testing cross validation", {
#   data("NAACproxydata")
#   data <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island","Placentia"))
#   # Cross Validation test
#   cv <- cross_val_check(raw_data = data,
#                         model_type ="ni_spline_st",
#                         n_iterations = 1,
#                         n_burnin = 1,
#                         n_thin = 1,
#                         n_chains = 1,
#                         n_fold = 2)
#
#
#   testthat::expect_true(is.list(cv))
# })
#
# # Testing the cross validation function for ni gam for 2 site
# testthat::test_that("Testing cross validation", {
#   data("NAACproxydata")
#   data <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island","Placentia"))
#   # Cross Validation test
#   cv <- cross_val_check(raw_data = data,
#                         model_type ="ni_gam_decomp",
#                         n_iterations = 1,
#                         n_burnin = 1,
#                         n_thin = 1,
#                         n_chains = 1,
#                         n_fold = 2)
#
#
#   testthat::expect_true(is.list(cv))
# })
#
