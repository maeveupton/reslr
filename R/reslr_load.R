#' Loading in data for the \code{reslr} package
#'
#' In this function, the data provided by the user is loaded into the package. The prerequisites of the input data structure has been given in the vignettes and the structure needs to be followed prior to running this function.
#' Within this function, a factor is created called "SiteName" which will be a combination of the Region and the Site name for each data site.
#' The prediction_grid_res corresponds to the output resolution the user requires. The default is 100 and can be altered based on the users requirements.
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
#' @param prediction_grid_res Resolution of grid. Predictions over every 50 years(default) can vary based on user preference as larger values will reduce computational run time.
#' @param include_tide_gauge Including tide gauge data from PSMSL website that is averaged over a decade using a rolling window
#' @param include_linear_rate User decides to include linear_rate and linear_rate_err
#' @param input_Age_type The inputted age in years "CE" or year "BCE"
#' @param list_preferred_TGs The user can supply the name or names of the preferred tide gauges
#' @param TG_minimum_dist_proxy The package finds the tide gauge closest to the proxy site
#' @param all_TG_1deg The package finds all tide gauges within 1 degree of the proxy site
#' @param sediment_average_TG Average the tide gauge data to make it comparable to accumulation rates of proxy records. The default averaging period for tide gauges is 10 years and the user can alter this.
#' @param detrend_data Detrend the data using the linear rate provided
#' @param core_col_year The year the sediment core was collected.
#'
#' @return A list containing data frame of data and prediction grid. The output of this function is two data frames, one with the data and one with the data_grid which represent a grid with evenly spaced time points.
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' reslr_load(data = data)
reslr_load <- function(data,
                       prediction_grid_res = 50,
                       include_tide_gauge = FALSE,
                       include_linear_rate = FALSE,
                       list_preferred_TGs = NULL,
                       TG_minimum_dist_proxy = FALSE,
                       all_TG_1deg = FALSE,
                       input_Age_type = "CE",
                       sediment_average_TG = 10,
                       detrend_data = FALSE,
                       core_col_year = NULL) {
  Age <- RSL <- Age_err <- RSL_err <- SiteName <- max_Age <- min_Age <- Longitude <- Latitude <- Site <- Region <- data_type_id <- ICE5_GIA_slope <- linear_rate_err <- linear_rate <- NULL

  # Dividing Age & Age_err by 1000 for easier calculations-----
  data <- data %>%
    dplyr::mutate(Age = Age / 1000) %>%
    dplyr::mutate(Age_err = Age_err / 1000)

  # Tidy Original data-------------------------------
  if (!("SiteName" %in% colnames(data))) {
    data <- data %>%
      dplyr::mutate(SiteName = as.factor(paste0(Site, ",", "\n", " ", Region)))
  } else {
    cat("Error: User must provide a column with site names and a column with region name. \n")
    stop()
  }
  if (input_Age_type == "BCE") {
    #cat("The inputed age value will be converted to units of Common Era. \n")
    data <- data %>%
      dplyr::mutate(Age = 1950/1000 - Age)
  } else {
    #cat("The inputed age value is units of Common Era. \n")
    data <- data
  }
  # Including no TG or linear rates
  if (include_tide_gauge == FALSE & include_linear_rate == FALSE) {
    data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
    #cat("No decadally averaged Tide gauge data or linear_rate included.\n")
    #cat("Note: Both are required for the ni_gam_decomp model \n")
  }

  # Including TG & no linear rates but forget to include TG method
  if (include_tide_gauge == TRUE &
    include_linear_rate == FALSE &
    is.null(list_preferred_TGs) == TRUE &
    TG_minimum_dist_proxy == FALSE &
    all_TG_1deg == FALSE) {
    message("Error: No tide gauge selection method chosen, please provide criteria for choosing preferred tide gauge. \n")
  }


  # Including TGs and no linear rates
  if (include_tide_gauge == TRUE & include_linear_rate == FALSE) {
    data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
    data <- clean_tidal_gauge_data(
      data = data,
      list_preferred_TGs = list_preferred_TGs,
      TG_minimum_dist_proxy = TG_minimum_dist_proxy,
      all_TG_1deg = all_TG_1deg,
      sediment_average_TG = sediment_average_TG
    )
    #cat("Note: No linear rate included. It is required for the ni_gam_decomp model \n")
    ##cat("Decadally averaged tide gauge data included by the package. \n")
  }



  # Including linear rates & no TG
  if (include_linear_rate == TRUE & include_tide_gauge == FALSE) {
    # Checking if user provided GIA rates----------
    if (!("linear_rate" %in% colnames(data) & "linear_rate_err" %in% colnames(data))) {
      lm_data_rates <- linear_reg_rates(data)
      data <- dplyr::left_join(data, lm_data_rates, by = "SiteName")
      cat("Package calculated linear_rate and linear_rate_err using the input data. \n")
      data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
      cat("Note: No Tide gauge data included. It is required for the ni_gam_decomp model \n")
    } else {
      data <- data
      cat("Package will use linear_rate and linear_rate_err provided by the user. \n")
      data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
     # cat("Note: No Tide gauge data included. It is required for the ni_gam_decomp model\n")
    }
  }

  # Including TG & linear rates but forget to include TG method
  if (include_tide_gauge == TRUE &
    include_linear_rate == TRUE &
    is.null(list_preferred_TGs) == TRUE &
    TG_minimum_dist_proxy == FALSE &
    all_TG_1deg == FALSE) {
    # message("Warning: No tide gauge selection method chosen. Select criteria to chose your prefered tide gauge")
    stop("Error: No tide gauge selection method chosen. Select criteria to chose your prefered tide gauge")
  }

  # Including linear rates & TG data
  if (include_linear_rate == TRUE & include_tide_gauge == TRUE) {
    data <- data %>% dplyr::mutate(data_type_id = "ProxyRecord")
    # Checking if user provided GIA rates----------
    if (!("linear_rate" %in% colnames(data) & "linear_rate_err" %in% colnames(data))) {
      lm_data_rates <- linear_reg_rates(data)
      data <- dplyr::left_join(data, lm_data_rates, by = "SiteName")
      cat("Package calculated linear_rate and linear_rate_err using the input data. \n")
    } else {
      data <- data
      cat("Package will use linear_rate and linear_rate_err provided by the user. \n")
    }
    # cat("Warning: provide a method of selecting tide gauge data \n")
    data <- clean_tidal_gauge_data(
      data = data,
      list_preferred_TGs = list_preferred_TGs,
      TG_minimum_dist_proxy = TG_minimum_dist_proxy,
      all_TG_1deg = all_TG_1deg,
      sediment_average_TG = sediment_average_TG
    )
    #cat("Decadally averaged tide gauge data included by the package. \n")
    #---Adding linear rates from ICE5G for TG-----
    data <- add_linear_rate(data = data)
    data <- data %>%
      dplyr::mutate(
        linear_rate = ifelse(data_type_id == "TideGaugeData", ICE5_GIA_slope, linear_rate),
        linear_rate_err = ifelse(data_type_id == "TideGaugeData", 0.3, linear_rate_err)
      )
    #cat("Tide Gauge data & linear_rate included \n")
  }

  # Detrending the data using GIA rates which is known as linear rate in my input dataframe
  if (detrend_data == TRUE) {
    if (is.null(data$linear_rate) & is.null(core_col_year)) {
      stop("Error: Linear rate for the proxy site must be included or update the setting linear_rate = TRUE. Must provide the year the core was collected \n")
    }
    browser()
    # Detrending the data and updating RSL to SL
    detrend_rate_val <- data %>%
      dplyr::filter(data_type_id == "ProxyRecord") %>%
      dplyr::select(linear_rate) %>% unique()
    detrend_rate <- rep(detrend_rate_val$linear_rate,nrow(data))
    data <- data %>%
      # Use the same rate for proxy and then for TGs
      #dplyr::group_by(SiteName) %>%
      dplyr::mutate(
        SL = (core_col_year / 1000 - Age) * detrend_rate + RSL,
        x_lwr_box = Age - Age_err,
        x_upr_box = Age + Age_err,
        y_upr  = RSL + RSL_err,
        y_lwr = RSL - RSL_err,
        # Detrending the uncertainties
        y_1_lwr = ((core_col_year / 1000 - (x_upr_box)) * detrend_rate) + (y_lwr),
        y_2_upr = ((core_col_year / 1000 - (x_lwr_box)) * detrend_rate) + (y_lwr),
        y_3_lwr = ((core_col_year / 1000 - (x_lwr_box)) * detrend_rate) + (y_upr),
        y_4_upr = ((core_col_year / 1000 - (x_upr_box)) * detrend_rate) + (y_upr),
        x_1_upr = Age + Age_err,
        x_2_lwr = Age - Age_err,
        x_3_lwr = Age - Age_err,
        x_4_upr = Age + Age_err)

    get_bounds <- data %>%
      select(y_1_lwr:x_4_upr) %>%
      mutate(obs_index = 1:n()) %>%
      pivot_longer(cols = y_1_lwr:x_4_upr,
                   names_to = "bounds",
                   values_to = "value") %>%
      mutate(bounds = replace(bounds, bounds %in% c("y_1_lwr","y_2_upr","y_3_lwr","y_4_upr"), "SL"),
             bounds = replace(bounds, bounds %in% c("x_1_upr","x_2_lwr","x_3_lwr","x_4_upr"), "Age"))

    x_bounds <- get_bounds %>%
      filter(bounds == "Age")

    y_bounds <- get_bounds %>%
      filter(bounds == "SL")

    detrend_data_un_box<- tibble(obs_index = x_bounds$obs_index,
                           Age = x_bounds$value,
                           SL = y_bounds$value)

  }




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

  times <- rep(seq(min(data$Age),
    max(data$Age),
    by = prediction_grid_res / 1000
  ), nrow(sites))
  sites <- sites[rep(seq_len(nrow(sites)),
    each = length(times %>% unique())
  ), ]
  data_grid_full <- dplyr::tibble(
    sites,
    Age = times
  )
  # Problem here for time models
  data_age_boundary <- data %>%
    dplyr::group_by(SiteName) %>%
    dplyr::summarise(
      max_Age = max(Age) + Age_err[1],
      min_Age = min(Age) - Age_err[length(Age_err)]
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
    ) %>%
    dplyr::select(!c(max_Age, min_Age)) %>%
    dplyr::arrange(Age)
  # Multiply by 1000 just keep it in right units
  data <- data %>%
    dplyr::mutate(Age = Age*1000, Age_err = Age_err*1000)
  data_grid <- data_grid %>%
    dplyr::mutate(Age = Age*1000)

  input_data <- base::list(
    data = data,
    data_grid = data_grid,
    prediction_grid_res = prediction_grid_res
  )
  if (detrend_data == TRUE) {
    input_data <- base::list(
      data = data,
      data_grid = data_grid,
      prediction_grid_res = prediction_grid_res,
      detrend_data_un_box = detrend_data_un_box
    )
    class(input_data) <- c("reslr_input", "detrend_data")
  } else {
    class(input_data) <- "reslr_input"
  }
  return(input_data)
}
