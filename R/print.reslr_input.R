#' Print reslr input object
#'
#' @param x An object of class \code{reslr_input}
#' @param ... Other argunemnts (not supported)
#'
#' @return A neat presentation of your input reslr object
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' reslr_input <- reslr_load(data = data)
#' print.reslr_input(x = reslr_input)
print.reslr_input <- function(x,...){
  cat("")
}
