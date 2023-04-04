#' Loading in data for the \code{reslr} package
#'
#' In this function, the data provided by the user is loaded into the package. The prerequisites of the input data structure has been given in the vignettes and the structure needs to be followed prior to running this function.
#' Within this function, a factor is created called "SiteName" which will be a combination of the Region and the Site name for each data site.
#' The n_prediction corresponds to the output resolution the user requires. The default is 100 and can be altered based on the users requirements.
#' The input Age column is examined to identify the input unit. The package has the ability to convert Before Common Era Age observations into Common Era Age estimates which is the preferred structure of the package.
#' Next, the user has to decided whether they require tide gauge data. The tide gauge data from the Permanent Service for Mean Sea Level online database is accessed in a temporary path.
#' The tide gauge data undergo a cleaning process in this function where flagged stations are removed as recommended by the online database.
#' From this the data is averaged using a rolling window over a decade to ensure it is comparable with proxy data and the tide gauge data is given an RSL uncertainty with is the standard deviation of the data over the decade and an Age error of 5 years corresponding to half a decade.
#' Then, the user selects their preferred tide gauge based on three criteria: 1.nearest tide gauge to the proxy site; 2. User supplies a list of names of preferred tide gauges; 3. all tide gauges within 1 degree are chosen.
#' The tide gauge dataframe is joined with the proxy dataframe with an ID column for data source, "ProxyRecord" or "TideGaugeData".
#' The other option the user must decide is whether to use linear rate which corresponds to an important physical process that impacts sea level observations which is glacial isostatic adjustment (GIA). The user can refer to the vignette about this process in detail.
#' For the linear_rate and its associated linear_rate_err, the user can provide these values as additional columns in the input dataframe. If they prefer, the package will calculate the linear_rate and the linear_rate_err using the data.
#'
#'
#' @param data The data of interest
#' @param n_prediction Predictions over every 100 years(default) can vary based on user preference
#' @param include_tide_gauge Including decadaly average tide gauge data from SMSL website
#' @param include_linear_rate User decides to include linear_rate and linear_rate_err
#' @param input_Age_type The inputted age in years CE or year BCE
#' @param list_preferred_TGs The user can supply the name or names of the preferred tide gauges
#' @param TG_minimum_dist_proxy The package finds the tide gauge closest to the proxy site
#' @param all_TG_1deg The package finds all tide gauges within 1 degree of the proxy site
#'
#' @return A list containing data frame of data and prediction grid. The output of this function is two data frames, one with the data and one with the data_grid which represent a grid with evenly spaced time points.
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
                       TG_minimum_dist_proxy = FALSE,
                       all_TG_1deg = FALSE,
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
    cat("\n The inputed age value will be converted to units of Common Era. \n")
    data <- data %>%
      dplyr::mutate(Age = 1950 - Age)
  } else {
    cat("\n The inputed age value is units of Common Era. \n")
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
    data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
    data <- clean_tidal_gauge_data(data = data,
                                   list_preferred_TGs = list_preferred_TGs,
                                   TG_minimum_dist_proxy= TG_minimum_dist_proxy,
                                   all_TG_1deg = all_TG_1deg)
    cat("Decadally averaged tide gauge data included by the package. \n")
    cat("Note: No linear rate included. It is required for the ni_gam_decomp model \n")
  }
  # else{
  #   data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
  #   cat("No decadally averaged Tide gauge data or linear_rate included.\n")
  #   cat("Note: Both are required for the ni_gam_decomp model \n")
  # }
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
  # else{
  #   cat("No linear_rate or linear_rate_err selected. \n")
  # }
  # Including linear rates & TG data
  if (include_linear_rate == TRUE & include_tide_gauge == TRUE){
    data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
    cat("Warning: provide a method of selecting tide gauge data \n")
    data <- clean_tidal_gauge_data(data = data,
                                   list_preferred_TGs = list_preferred_TGs,
                                   TG_minimum_dist_proxy = TG_minimum_dist_proxy,
                                   all_TG_1deg = all_TG_1deg)
    cat("Decadally averaged tide gauge data included by the package. \n")
    # Checking if user provided GIA rates----------
    if (!("linear_rate" %in% colnames(data) & "linear_rate_err" %in% colnames(data))) {
      lm_data_rates <- linear_reg_rates(data)
      data <- dplyr::left_join(data, lm_data_rates, by = "SiteName")
      cat("Package calculated linear_rate and linear_rate_err using the input data. \n")
    }
    else {
      data <- data
      cat("Package will use linear_rate and linear_rate_err provided by the user. \n")
    }
    #---Adding linear rates from ICE5G for TG-----
    data <- add_linear_rate(data = data)
    data <- data %>%
          dplyr::mutate(
            linear_rate = ifelse(data_type_id == "TideGaugeData", ICE5_GIA_slope, linear_rate),
            linear_rate_err = ifelse(data_type_id == "TideGaugeData", 0.3, linear_rate_err)
          )
    cat("Tide Gauge data & linear_rate included")
  }
#browser()
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
