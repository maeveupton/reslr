#' Print a reslr output object which is created from the \code{reslr_load} function.
#'
#' In this function, the reslr input object is printed. This is a high-level summary which provides the number of observations and the number of sites utilised in the dataset.
#'
#' @param x An object of class \code{reslr_input}
#' @param ... Other arguments (not supported)
#'
#' @return A neat presentation of your input reslr object
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' reslr_input <- reslr_load(data = data)
#' print(x = reslr_input)
print.reslr_input <- function(x,
                              ...) {
  SiteName <- data_type_id <- NULL
  data <- x$data
  n_obs <- nrow(data)
  n_sites <- length(data$SiteName %>% unique())
  n_proxy <- data %>%
    dplyr::filter(data_type_id == "ProxyRecord") %>%
    dplyr::select(SiteName, data_type_id) %>%
    unique() %>%
    nrow()
  n_tide_gauge <- n_sites - n_proxy

  cat("This is a valid reslr input object with ")
  cat(paste(n_obs, "observations "))
  cat("and ", paste(n_sites), "site(s).\n")
  cat("There are ", paste(n_proxy), " proxy site(s) ")

  cat("and ", paste(n_tide_gauge), " tide gauge site(s).\n")

  if("Age_type" %in% colnames(data)){
    cat("The age units are; Before Present Era. \n")
  }
  else{
    cat("The age units are; Common Era. \n")}

  if(n_tide_gauge == 0){
    cat("Decadally averaged tide gauge data was not included. It is recommended for the ni_gam_decomp model \n")
  }
  else{
    cat("Decadally averaged tide gauge data included by the package. \n")
  }
  if("linear_rate" %in% colnames(data) & "linear_rate_err" %in% colnames(data)){
    cat("The linear_rate and linear_rate_err has been included. \n")
  }
  else{
    cat("The linear_rate or linear_rate_err was not included. It is required for the ni_gam_decomp model \n")
  }
  if(inherits(x,"detrend_data")){
    cat("Data has been detrended.\n")
  }
}
