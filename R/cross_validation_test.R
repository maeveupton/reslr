#' This function examines the number of knots used to create the spline model fit.
#'
#'
#' @param object Output object from the \code{\link{reslr_mcmc}}
#' @param ... Not in use
#'
#' @return A list containing convergence diagnostics and parameter estimates for the output.
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' input_data <- reslr_load(data = data)
#' jags_output <- reslr_mcmc(input_data = input_data, model_type = "ni_spline_t")
#' cross_validation_test(object = jags_output)
cross_validation_test <- function(object,
                                  ...){

}
