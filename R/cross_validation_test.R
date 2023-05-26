#' This function examines the number of knots used to create the spline model fit.
#'
#'
#' @param object Output object from the \code{\link{reslr_load}}
#' @param
#' @param ... Not in use
#'
#' @return A list containing convergence diagnostics and parameter estimates for the output.
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' input_data <- reslr_load(data = data)
#' cross_validation_test(object = input_data)
cross_validation_test <- function(object,
                                  spline
                                  ...){

}
