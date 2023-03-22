#' Loading input data
#'
#' @param data The data of interest
#' @param n_prediction Predictions over every 100 years(default) can vary based on user preference
#' @param include_tide_gauge Including decadaly average tide gauge data from PSMSL website
#' @param include_linear_rate User decides to include linear_rate and linear_rate_err
#' @param input_Age_type The inputted age in years CE or year BCE
#'
#' @return A list containing data frame of data and prediction grid
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' reslr_load(data = data)
reslr_load <- function(data,
                       n_prediction = 100,
                       include_tide_gauge = FALSE,
                       include_linear_rate = FALSE,
                       input_Age_type = "CE") {
  Age <- RSL <- Age_err <- RSL_err <- SiteName <- max_Age <- min_Age <- Longitude <- Latitude <- Site <- Region <- data_type_id <- ICE5_GIA_slope <- linear_rate_err <- linear_rate <- NULL

  # Tidy Original data-------------------------------
  if (!("SiteName" %in% colnames(data))) {
    data <- data %>%
      dplyr::mutate(SiteName = as.factor(paste0(Site, ",", "\n", " ", Region)))
  } else {
    cat("User must provide a site name and a region name. \n")
  }
  if (input_Age_type == "BCE") {
    cat("The inputed age value will be converted to units of Common Era. \n")
    data <- data %>%
      dplyr::mutate(Age = 1950 - Age)
  } else {
    cat("The inputed age value is units of Common Era. \n")
    data <- data
  }

  # Checking if user provided GIA rates----------
  if (include_linear_rate == TRUE){
    if (!("linear_rate" %in% colnames(data) & "linear_rate_err" %in% colnames(data))) {
      lm_data_rates <- linear_reg_rates(data)
      data <- dplyr::left_join(data, lm_data_rates, by = "SiteName")
      cat("Package calculated linear_rate and linear_rate_err using the input data. \n")
    } else {
      data <- data
      cat("Package will use linear_rate and linear_rate_err provided by the user. \n")
    }
  }
  else{
    cat("No linear_rate or linear_rate_err selected. \n")
  }
  # Include tide gauge data--------------------
  if (include_tide_gauge == "TRUE") {
    if (!("data_type_id" %in% colnames(data))) {
      data <- clean_tidal_gauge_data(data = data)
      #---Adding linear rates from ICE5G for TG-----
      data <- add_linear_rate(data = data)
      data <- data %>%
        dplyr::mutate(
          linear_rate = ifelse(data_type_id == "TideGaugeData", ICE5_GIA_slope, linear_rate),
          linear_rate_err = ifelse(data_type_id == "TideGaugeData", 0.3, linear_rate_err)
        )
      cat("Tide Gauge data included by the package. \n")
    }
  } else {
    data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
    cat("No Tide Gauge data included. \n")
    # cat("Decadally average tide gauge data provided by user along with GIA rate and associated uncertainty")}
  }
  # else{
  #   cat("No Tide Gauge data included")
  # }


  # Prediction dataframe-------------------------------------
  sites <- data %>%
    dplyr::select(
      Longitude,
      Latitude,
      SiteName,
      dplyr::contains("linear_rate"),
      dplyr::contains("linear_rate_err"),
      dplyr::contains("data_type_id")
    ) %>%
    unique()
  times <- rep(seq(min(data$Age) - n_prediction / 1000,
    max(data$Age) + n_prediction / 1000,
    by = n_prediction / 1000
  ), nrow(sites))
  sites <- sites[rep(seq_len(nrow(sites)),
    each = length(times %>% unique())
  ), ]
  data_grid_full <- dplyr::tibble(
    Age = times,
    Longitude = sites$Longitude,
    Latitude = sites$Latitude,
    SiteName = sites$SiteName,
    linear_rate = sites$linear_rate,
    linear_rate_err = sites$linear_rate_err,
    data_type_id = sites$data_type_id
  )
  # if(include_tide_gauge == "TRUE") {
  # data_age_boundary <- data %>%
  #   dplyr::group_by(SiteName,data_type_id) %>%
  #   dplyr::summarise(max_Age = ifelse(data_type_id == "ProxyRecordData",
  #                                     max(Age)+n_prediction/1000,max(Age)+0.001),
  #                    min_Age = ifelse(data_type_id == "ProxyRecordData",
  #                                     (min(Age)-n_prediction/1000),(min(Age)-0.001))) %>%
  #   unique()
  # }
  # else{
  data_age_boundary <- data %>%
    dplyr::group_by(SiteName) %>%
    dplyr::summarise(
      max_Age = max(Age) + 10/1000,# + (n_prediction / 1000),
      min_Age = min(Age) - 10/1000# - (n_prediction / 1000)
    ) %>%
    unique()
  # }

  data_age_boundary_test <-
    data_age_boundary %>% dplyr::mutate(max_age = max_Age * 1000, min_age = min_Age * 1000)
  # Filtering prediction grids to just cover the data
  data_grid <- data_grid_full %>%
    dplyr::left_join(data_age_boundary, by = "SiteName") %>%
    dplyr::group_by(SiteName) %>%
    dplyr::filter(Age >= (min_Age) & Age <= (max_Age)) %>%
    dplyr::tibble()
  data_grid_test <- data_grid %>%
    dplyr::group_by(SiteName) %>%
    dplyr::summarise(minAge_range = min(Age) * 1000, maxAge_range = max(Age) * 1000)

  # # Calculating GIA using linear regression through the data ------------------
  # if(GIA_rate_provided == "TRUE" & GIA_rate_sd_provided == "TRUE"){
  #
  #   cat("Using rate and associated uncertainty of the rate provided by the user")
  # }
  # else{
  #   cat("No rate or associated uncertainty provided, package will calculate it using the data")
  # }
  # if (calculate_GIA_rate == "TRUE") {
  #   lm_data_rates <- linear_reg_rates(data)
  #   data <- dplyr::left_join(data, lm_data_rates, by = "SiteName")
  #   cat("Calculated GIA rate and uncertainty using the input data")
  # }

  # Ensuring SiteName is a factor
  data <- data %>%
    dplyr::mutate(
      SiteName = as.factor(SiteName),
      data_type_id = as.factor(data_type_id)
    )
  data_grid <- data_grid %>%
    dplyr::mutate(
      SiteName = as.factor(SiteName),
      data_type_id = as.factor(data_type_id)
    ) %>% dplyr::select(!c(max_Age,min_Age))

  input_data <- base::list(
    data = data,
    data_grid = data_grid
  )
  class(input_data) <- "reslr_input"
  return(input_data)
}
