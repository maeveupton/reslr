# testthat::context("reslr_load")
# library(reslr)
#


# Full dataset test
testthat::test_that("Full data set example", {
  data("NAACproxydata")
  data_testsite <- NAACproxydata
  # Load in with reslr_load
  reslr_1 <-
    reslr_load(
      data = data_testsite,
      prediction_grid_res = 100,
      include_tide_gauge = FALSE,
      include_linear_rate = FALSE,
      list_preferred_TGs = NULL,
      TG_minimum_dist_proxy = FALSE,
      all_TG_1deg = FALSE,
      input_age_type = "CE"
    )
  testthat::expect_s3_class(reslr_1, "reslr_input")
  testthat::expect_true(is.data.frame(reslr_1$data))
  testthat::expect_true(is.data.frame(reslr_1$data_grid))
})

# Filtering for 1 site to increase speed
testthat::test_that("One site example", {
  data("NAACproxydata")
  data_testsite <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
  # Load in with reslr_load
  reslr_1 <-
    reslr_load(
      data = data_testsite,
      prediction_grid_res = 100,
      include_tide_gauge = FALSE,
      include_linear_rate = FALSE,
      list_preferred_TGs = NULL,
      TG_minimum_dist_proxy = FALSE,
      all_TG_1deg = FALSE,
      input_age_type = "CE"
    )
  testthat::expect_s3_class(reslr_1, "reslr_input")
  testthat::expect_true(is.data.frame(reslr_1$data))
  testthat::expect_true(is.data.frame(reslr_1$data_grid))
})

# Testing function for additional tide gauge data which is closest
testthat::test_that("Simplest example with closest tide gauge", {
  data("NAACproxydata")
  data_testsite <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")

  # Load in with reslr_load
  reslr_1 <-
    reslr_load(
      data = data_testsite,
      prediction_grid_res = 100,
      include_tide_gauge = TRUE,
      include_linear_rate = FALSE,
      list_preferred_TGs = NULL,
      TG_minimum_dist_proxy = TRUE,
      all_TG_1deg = FALSE,
      input_age_type = "CE"
    )
  testthat::expect_s3_class(reslr_1, "reslr_input")
  testthat::expect_true(is.data.frame(reslr_1$data))
  testthat::expect_true(is.data.frame(reslr_1$data_grid))
})

# Testing function for additional tide gauge data from a list
testthat::test_that("Simplest example with list of tide gauges", {
  data("NAACproxydata")
  data_testsite <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")

  # Load in with reslr_load
  reslr_1 <-
    reslr_load(
      data = data_testsite,
      prediction_grid_res = 100,
      include_tide_gauge = TRUE,
      include_linear_rate = FALSE,
      list_preferred_TGs = c("ARGENTIA"),
      TG_minimum_dist_proxy = FALSE,
      all_TG_1deg = FALSE,
      input_age_type = "CE"
    )
  testthat::expect_s3_class(reslr_1, "reslr_input")
  testthat::expect_true(is.data.frame(reslr_1$data))
  testthat::expect_true(is.data.frame(reslr_1$data_grid))
})

# Testing function for additional tide gauge data all within 1 degree
testthat::test_that("Simplest example with all tide gauges within 1 degree", {
  data("NAACproxydata")
  data_testsite <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")

  # Load in with reslr_load
  reslr_1 <-
    reslr_load(
      data = data_testsite,
      prediction_grid_res = 100,
      include_tide_gauge = TRUE,
      include_linear_rate = FALSE,
      list_preferred_TGs = NULL,
      TG_minimum_dist_proxy = FALSE,
      all_TG_1deg = TRUE,
      input_age_type = "CE"
    )
  testthat::expect_s3_class(reslr_1, "reslr_input")
  testthat::expect_true(is.data.frame(reslr_1$data))
  testthat::expect_true(is.data.frame(reslr_1$data_grid))
})

# Testing function for including linear rate
testthat::test_that("Simplest example with linear rate", {
  data("NAACproxydata")
  data_testsite <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")

  # Load in with reslr_load
  reslr_1 <-
    reslr_load(
      data = data_testsite,
      prediction_grid_res = 100,
      include_tide_gauge = FALSE,
      include_linear_rate = TRUE,
      list_preferred_TGs = NULL,
      TG_minimum_dist_proxy = FALSE,
      all_TG_1deg = FALSE,
      input_age_type = "CE"
    )
  testthat::expect_s3_class(reslr_1, "reslr_input")
  testthat::expect_true(is.data.frame(reslr_1$data))
  testthat::expect_true(is.data.frame(reslr_1$data_grid))
})

# Testing function for including linear rate & closest tide gauge
testthat::test_that("Simplest example with linear rate and closes tide gauge", {
  data("NAACproxydata")
  data_testsite <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")

  # Load in with reslr_load
  reslr_1 <-
    reslr_load(
      data = data_testsite,
      prediction_grid_res = 100,
      include_tide_gauge = TRUE,
      include_linear_rate = TRUE,
      list_preferred_TGs = NULL,
      TG_minimum_dist_proxy = TRUE,
      all_TG_1deg = FALSE,
      input_age_type = "CE"
    )
  testthat::expect_s3_class(reslr_1, "reslr_input")
  testthat::expect_true(is.data.frame(reslr_1$data))
  testthat::expect_true(is.data.frame(reslr_1$data_grid))
})
