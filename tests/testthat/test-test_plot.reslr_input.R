# testthat::context("plot.reslr_input")
# co <- function(expr) capture.output(expr, file = "NUL")
#
# library(reslr)
#
# data("NAACproxydata")
#
# # Load in with reslr_load
# reslr_1 <-
#   reslr_load(
#     data = NAACproxydata,
#     n_prediction = 100,
#     tide_gauge_included = FALSE,
#     input_Age_type = "CE"
#   )
#
# # ??
# testthat::test_that("basic reslr_input plot", {
#   p <- plot(reslr_1)
#   testthat::expect_type(p,"ggplot")
#   #vdiffr::expect_doppelganger("reslr_input", p)
#   # p <- plot(reslr, colour = FALSE)
#   # testthat::expect_doppelganger("simmr_input_no_col", p)
#   # p <- plot(simmr_1, tracers = c(2, 1))
#   # testthat::expect_doppelganger("simmr_input_rev_tracers", p)
# })
