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
                              ...){
  data <- x$data
  n_obs <- nrow(data)
  n_sites <- length(data$SiteName %>% unique)
  # n_proxy <- data %>%
  #   dplyr::select(SiteName,data_type_id) %>%
  #   unique() %>%
  #   length()

  # if(data$data_type == "TideGaugeData"){
  #   n_tg <- data %>%
  #     dplyr::filter(data_type_id == "TideGaugeData") %>%
  #     unique() %>% length()
  # }
  # else{
  #   n_tg = 0
  # }


  cat("This is a valid reslr input object with ")
  cat(paste(n_obs, "observations "))
  cat("and ",paste(n_sites),"sites.\n")
  #cat("There are ", paste(n_proxy)," proxy sites ")
  #cat("and ", paste(n_tg)," tide gauge sites.\n")
}
