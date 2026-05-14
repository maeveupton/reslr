co <- function(expr) capture.output(expr, file = "NULL")

# Testing plotting functions for 1 site
data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
reslr_input_1 <- reslr_load(
  data = data,
  prediction_grid_res = 100,
  include_tide_gauge = FALSE,
  include_linear_rate = FALSE,
  input_age_type = "CE"
)

testthat::test_that("Basic reslr_input plot", {
  p <- plot(reslr_input_1)
  testthat::expect_s3_class(reslr_input_1, c("reslr_input"))
  vdiffr::expect_doppelganger("inputplot1site", p)
})

# Testing all loading options for plotting functions for 1 site and tide gauge
reslr_input_2 <- reslr_load(
  data = data,
  prediction_grid_res = 100,
  include_tide_gauge = TRUE,
  TG_minimum_dist_proxy = TRUE,
  include_linear_rate = TRUE,
  input_age_type = "CE"
)

testthat::test_that("Basic reslr_input plot with tide gauges", {
  p2 <- plot(reslr_input_2,
    plot_tide_gauges = TRUE
  )
  testthat::expect_s3_class(reslr_input_2, c("reslr_input"))
  vdiffr::expect_doppelganger("inputplotTG1site", p2)
})


# Testing plotting functions for multiple sites
multidata <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island", "Barn Island", "Nassau"))
reslr_input_3 <- reslr_load(
  data = multidata,
  prediction_grid_res = 100,
  include_tide_gauge = FALSE,
  include_linear_rate = FALSE,
  input_age_type = "CE"
)

testthat::test_that("Basic reslr_input plot for multiple sites", {
  p3 <- plot(reslr_input_3)
  testthat::expect_s3_class(reslr_input_3, c("reslr_input"))
  vdiffr::expect_doppelganger("inputplot3sites", p3)
})

# Testing plotting functions for multiple sites and tide gauges
multidata <- NAACproxydata %>% dplyr::filter(Site %in% c("Cedar Island", "Barn Island", "Nassau"))
reslr_input_4 <- reslr_load(
  data = multidata,
  prediction_grid_res = 100,
  include_tide_gauge = TRUE,
  include_linear_rate = FALSE,
  TG_minimum_dist_proxy = TRUE,
  input_age_type = "CE"
)

testthat::test_that("Basic reslr_input plot for multiple sites and tide gauges", {
  p4 <- plot(reslr_input_4)
  testthat::expect_s3_class(reslr_input_4, c("reslr_input"))
  vdiffr::expect_doppelganger("inputplot3sitesTG", p4)
})
