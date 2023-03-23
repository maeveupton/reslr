#' Loading input data
#'
#' @param data The data of interest
#' @param n_prediction Predictions over every 100 years(default) can vary based on user preference
#' @param include_tide_gauge Including decadaly average tide gauge data from PSMSL website
#' @param include_linear_rate User decides to include linear_rate and linear_rate_err
#' @param input_Age_type The inputted age in years CE or year BCE
#' @param list_preferred_TGs The user can supply the name or names of the preferred tide gauges
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
                       list_preferred_TGs = NULL,
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
  # Including no TG or linear rates
  if(include_tide_gauge == FALSE & include_linear_rate == FALSE){
    data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
    cat("No decadally averaged Tide gauge data or linear_rate included.\n")
    cat("Note: Both are required for the ni_gam_decomp model \n")
  }
  # Including TGs and no linear rates
  if(include_tide_gauge == TRUE & include_linear_rate == FALSE){
    data <- clean_tidal_gauge_data(data = data)#$additional_datasets$data
    cat("Decadally averaged tide gauge data included by the package. \n")
    cat("Note: No linear rate included. It is required for the ni_gam_decomp model \n")
  }
  # Including linear rates & no TG
  if (include_linear_rate == TRUE & include_tide_gauge == FALSE){
    # Checking if user provided GIA rates----------
    if (!("linear_rate" %in% colnames(data) & "linear_rate_err" %in% colnames(data))) {
      lm_data_rates <- linear_reg_rates(data)
      data <- dplyr::left_join(data, lm_data_rates, by = "SiteName")
      cat("Package calculated linear_rate and linear_rate_err using the input data. \n")
      data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
      cat("Note: No Tide gauge data included. It is required for the ni_gam_decomp model \n")
    }
    else {
      data <- data
      cat("Package will use linear_rate and linear_rate_err provided by the user. \n")
      data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
      cat("Note: No Tide gauge data included. It is required for the ni_gam_decomp model\n")
    }
  }
  # FINISH
  # Including linear rates & TG data
  if (include_linear_rate == TRUE & include_tide_gauge == TRUE){
    # Checking if user provided GIA rates----------
    if (!("linear_rate" %in% colnames(data) & "linear_rate_err" %in% colnames(data))) {
      lm_data_rates <- linear_reg_rates(data)
      data <- dplyr::left_join(data, lm_data_rates, by = "SiteName")
      cat("Package calculated linear_rate and linear_rate_err using the input data. \n")
      #data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
      cat("No Tide gauge data included \n")
    }
    else {
      data <- data
      cat("Package will use linear_rate and linear_rate_err provided by the user. \n")
      #data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
    }
    TG_data <- clean_tidal_gauge_data(data = data)
    # Annual TG data?
    data <- clean_tidal_gauge_data(data = data)#$additional_datasets$data

    #---Adding linear rates from ICE5G for TG-----
    TG_lin_data <- add_linear_rate(data = TG_data)
    data <- data %>% merge(data, TG_lin_data)
    data <- data %>%
          dplyr::mutate(
            linear_rate = ifelse(data_type_id == "TideGaugeData", ICE5_GIA_slope, linear_rate),
            linear_rate_err = ifelse(data_type_id == "TideGaugeData", 0.3, linear_rate_err)
          )
    cat("Decadally averaged tide gauge data included by the package. \n")

  }

  # else{
  #   cat("No linear_rate or linear_rate_err selected. \n")
  # }

  # # Calculating the linear rates for the proxy sites
  # if (include_linear_rate == TRUE & include_tide_gauge == FALSE){
  #   # Checking if user provided GIA rates----------
  #   if (!("linear_rate" %in% colnames(data) & "linear_rate_err" %in% colnames(data))) {
  #     lm_data_rates <- linear_reg_rates(data)
  #     data <- dplyr::left_join(data, lm_data_rates, by = "SiteName")
  #     cat("Package calculated linear_rate and linear_rate_err using the input data. \n")
  #   }
  #   else {
  #     data <- data
  #     cat("Package will use linear_rate and linear_rate_err provided by the user. \n")
  #   }
  # }
  # else{
  #   cat("No linear_rate or linear_rate_err selected. \n")
  # }

  # # Include tide gauge data with no linear rates--------------------
  # if (include_tide_gauge == TRUE & include_linear_rate == FALSE) {
  #   if (!("data_type_id" %in% colnames(data))) {
  #     data <- clean_tidal_gauge_data(data = data)
  #     cat("Decadally averaged tide Gauge data included by the package. \n")
  #   }
  #   #else {
  #   #  data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
  #   #  cat("No Tide Gauge data included. \n")
  #   #}
  #
  # }
  # else{
  #   cat("User is using Proxy Records only")
  # }

  # # Include TG data & linear rates
  # if(include_tide_gauge == TRUE & include_linear_rate == TRUE){
  #   # Checking if user provided GIA rates----------
  #   if (!("linear_rate" %in% colnames(data) & "linear_rate_err" %in% colnames(data))) {
  #     lm_data_rates <- linear_reg_rates(data)
  #     data <- dplyr::left_join(data, lm_data_rates, by = "SiteName")
  #     cat("Package calculated linear_rate and linear_rate_err using the input data. \n")
  #   }
  #   else {
  #     data <- data
  #     cat("Package will use linear_rate and linear_rate_err provided by the user. \n")
  #   }
  #
  #   #if (!("data_type_id" %in% colnames(data))) {
  #     TG_data <- clean_tidal_gauge_data(data = data)
  #     #---Adding linear rates from ICE5G for TG-----
  #     TG_lin_data <- add_linear_rate(data = TG_data)
  #
  #     data <- data %>%
  #       dplyr::mutate(
  #         linear_rate = ifelse(data_type_id == "TideGaugeData", ICE5_GIA_slope, linear_rate),
  #         linear_rate_err = ifelse(data_type_id == "TideGaugeData", 0.3, linear_rate_err)
  #       )
  #   cat("Decadally averaged tide Gauge data and linear_rate included by the package. \n")
  #  # }
  # }
  # else{
  #     cat("No decadally averaged Tide gauge data or linear_rate included.\n")
  #   }



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
  times <- rep(seq(min(data$Age) ,
    max(data$Age),
    by = n_prediction / 1000
  ), nrow(sites))
  sites <- sites[rep(seq_len(nrow(sites)),
    each = length(times %>% unique())
  ), ]
  data_grid_full <- dplyr::tibble(
    sites,
    Age = times#,
    # Longitude = sites$Longitude,
    # Latitude = sites$Latitude,
    # SiteName = sites$SiteName,
    # linear_rate = sites$linear_rate,
    # linear_rate_err = sites$linear_rate_err,
    # data_type_id = sites$data_type_id
  )

  data_age_boundary <- data %>%
    dplyr::group_by(SiteName) %>%
    dplyr::summarise(
      max_Age = max(Age)  + (n_prediction / 1000),
      min_Age = min(Age) -(n_prediction / 1000)
    ) %>%
    unique()
  # Filtering prediction grids to just cover the data
  data_grid <- data_grid_full %>%
    dplyr::left_join(data_age_boundary, by = "SiteName") %>%
    dplyr::group_by(SiteName) %>%
    dplyr::filter(Age >= (min_Age) & Age <= (max_Age)) %>%
    dplyr::tibble() %>%
    dplyr::group_by(SiteName) %>%
    dplyr::mutate(Age = replace(Age, Age == min(Age), unique(min_Age))) %>%
    dplyr::mutate(Age = replace(Age, Age == max(Age), unique(max_Age)))

  # data_grid <- data_grid_full %>%
  #   dplyr::left_join(data_age_boundary, by = "SiteName") %>%
  #   dplyr::group_by(SiteName) %>%
  #   dplyr::filter(Age >= (min_Age) & Age <= (max_Age)) %>%
  #   dplyr::tibble()
  data_grid_test <- data_grid %>%
    dplyr::group_by(SiteName) %>%
    dplyr::summarise(minAge_range = min(Age) * 1000, maxAge_range = max(Age) * 1000)

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
