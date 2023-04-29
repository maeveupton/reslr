#' A function that will create a plot of model fits for the output of the function \code{reslr_mcmc}.
#'
#' @param output_dataframes These are dataframes created for plotting and are outputs from the \code{reslr_mcmc} function.
#' @param data This is the input dataset stored in a list created in the \code{reslr_mcmc} function.
#' @param plot_tide_gauges If plotting tide gauges add them onto the end of the proxy record
#' @param caption Caption of the title depending on the model type
#'
#' @return The plot of the model fit
#' @noRd
create_model_fit_plot <- function(output_dataframes,
                                  data,
                                  plot_tide_gauges = FALSE,
                                  model_caption) {
  data_type_id <- pred <- lwr <- upr <- Age <- RSL <- Age_err <- RSL_err <- SiteName <- Longitude <- Latitude <- NULL
  if (plot_tide_gauges == FALSE) {
    # Plot
    plot <-
      ggplot2::ggplot() +
      ggplot2::geom_rect(data = data, ggplot2::aes(
        xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
        ymin = RSL - RSL_err, ymax = RSL + RSL_err, fill = "Uncertainty",
      ), alpha = 0.4) +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.5
      ) +
      ggplot2::geom_line(
        data = output_dataframes,
        ggplot2::aes(x = Age * 1000, y = pred, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = output_dataframes,
        ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age * 1000, fill = "CI"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = output_dataframes,
      #   ggplot2::aes(y = pred, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
      # ) +
      ggplot2::xlab("Age (CE)") +
      ggplot2::ylab("Relative Sea Level (m)") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 15),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        axis.text = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 10)
      ) +
      ggplot2::theme(
        strip.text.x = ggplot2::element_text(size = 10),
        strip.background = ggplot2::element_rect(fill = c("white"))
      ) +
      ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
      ggplot2::labs(colour = "") +
      ggplot2::scale_fill_manual("",
        values = c(
          "Uncertainty" = ggplot2::alpha("grey", 0.3),
          "CI" = ggplot2::alpha("purple3", 0.2) #
          # "50" = ggplot2::alpha("purple3", 0.3)
        ),
        labels = c(
          CI = paste0(unique(output_dataframes$CI)," Credible Interval"),
          #"95% Credible Interval",
          expression(paste("1-sigma Error"))
          # , "50% Credible Interval"
        )
      ) +
      ggplot2::scale_colour_manual("",
        values = c("black" = "black", "mean" = "purple3"),
        labels = c("Data", "Posterior Fit")
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(override.aes = list(
          alpha = c(0.4, 0.2), # , 0.4),
          size = 1
        )),
        colour = ggplot2::guide_legend(override.aes = list(
          linetype = c(0, 1),
          shape = c(16, NA),
          size = 2
        ))
      ) +
      ggplot2::facet_wrap(~SiteName)+
      ggplot2::labs(caption = model_caption)
  } else {
    # Plot
    plot <-
      ggplot2::ggplot() +
      ggplot2::geom_rect(data = data, ggplot2::aes(
        xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
        ymin = RSL - RSL_err, ymax = RSL + RSL_err, fill = "Uncertainty",
      ), alpha = 0.4) +
      #ggplot2::ggtitle(paste0("Proxy data & Tide gauge data from:",unique(as.factor(data$SiteName))))+
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.5
      ) +#, shape = data_type_id
      ggplot2::geom_line(
        data = output_dataframes,
        ggplot2::aes(x = Age * 1000, y = pred, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = output_dataframes,
        ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age * 1000, fill = "CI"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = output_dataframes,
      #   ggplot2::aes(y = pred, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
      # ) +

      ggplot2::xlab("Age (CE)") +
      ggplot2::ylab("Relative Sea Level (m)") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 15),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        axis.text = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 10)
      ) +
      ggplot2::theme(
        strip.text.x = ggplot2::element_text(size = 10),
        strip.background = ggplot2::element_rect(fill = c("white"))
      ) +
      ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
      ggplot2::labs(colour = "") +
      ggplot2::scale_fill_manual("",
        values = c(
          "Uncertainty" = ggplot2::alpha("grey", 0.3),
          CI = ggplot2::alpha("purple3", 0.2) #
          # "50" = ggplot2::alpha("purple3", 0.3)
        ),
        labels = c(
          #"95% Credible Interval",
          CI = paste0(unique(output_dataframes$CI)," Credible Interval"),
          "Uncertainty" = expression(paste("1-sigma Error"))
          # , "50% Credible Interval"
        )
      ) +
      ggplot2::scale_colour_manual("",
        values = c("black" = "black", "mean" = "purple3"),
        labels = c("Data", "Posterior Fit")
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(override.aes = list(
          #alpha = c(0.4, 0.2), # , 0.4),
          size = 1
        )),
        colour = ggplot2::guide_legend(override.aes = list(
          linetype = c(0, 1),
          shape = c(16, NA),
          size = 2
        ))
      ) +
    ggplot2::facet_wrap(~SiteName)+
    ggplot2::labs(caption = model_caption)
  }
  return(plot)
}

#' In this function, tide gauge data from the Permanent Service for Mean Sea Level online database is accessed in a temporary path.
#' The tide gauge data undergo a cleaning process in this function where flagged stations are removed as recommended by the online database.
#' Next, the data is averaged using a rolling window over a decade to ensure it is comparable with proxy data and the tide gauge data is given an RSL uncertainty with is the standard deviation of the data over the decade and an Age error of 5 years corresponding to half a decade.
#' Then, the user selects their preferred tide gauge based on three criteria: 1.nearest tide gauge to the proxy site; 2. User supplies a list of names of preferred tide gauges; 3. all tide gauges within 1 degree are chosen.
#' The tide gauge dataframe is joined with the proxy dataframe with an ID column for data source, "ProxyRecord" or "TideGaugeData"
#'
#' @param data Input data
#' @param list_preferred_TGs user can supply the name or names of the preferred tide gauges
#' @param TG_minimum_dist_proxy The user wants the tide gauge closest to the proxy site
#' @param all_TG_1deg The user wants all tide gauges within 1 degree of the proxy site
#' @param rolling_window_average A rolling window that averages tide gauge data to make it comparable to accumulation rates of proxy records. The default averaging period for tide gauges is 10 years and the user can alter this.
#' @noRd
clean_tidal_gauge_data <- function(data,
                                   list_preferred_TGs = NULL,
                                   TG_minimum_dist_proxy = FALSE,
                                   all_TG_1deg = FALSE,
                                   rolling_window_average = 10) {
  Age_epoch_id <- LongLat <- rolling_avg <- median <- nearest_proxy_site <- RSL_annual <- TG_min_dist1 <- minimum_dist <- nearest_TG <- rows_site <- site <- min_dist1 <- stationflag <- name <- sd <- sd_TG <- n_obs_by_site <- RSL_offset <- data_type_id <- decade <- decade_meanRSL <- Age <- RSL <- Age_err <- RSL_err <- linear_rate <- linear_rate_err <- SiteName <- Longitude <- Latitude <- id <- NULL
  # Using data from PSMSL website for annual tide gauge data----------------------------------
  # Set up the URL for downloading the data
  url <- "https://psmsl.org/data/obtaining/rlr.annual.data/rlr_annual.zip"

  # Create a temporary file
  temp_file <- tempfile()

  # Download the file and save it to the temporary file
  utils::download.file(url, destfile = temp_file)

  # Unzip the data file to a temporary directory
  temp_dir <- tempfile()
  utils::unzip(temp_file, exdir = temp_dir)

  ### ------------Loop to open all RSL & Age data files------------
  read_plus <- function(flnm) {
    data.table::fread(flnm, sep = ";") %>% # fread quicker way to read in & allows for ; to be used
      dplyr::mutate(filename = flnm) # allows you to include the file name as id
  }
  # Warnings: there are some files without data
  suppressWarnings(
    temp_SL <-
      list.files(
        path = file.path(temp_dir, "rlr_annual", "data"),
        # path = "rlr_annual/data",
        pattern = "*.rlrdata",
        full.names = T
      ) %>%
      purrr::map_df(~ read_plus(.)) %>%
      dplyr::tibble()
  )

  colnames(temp_SL) <- c("Age", "RSL", "flag_attention_1", "flag_attention_2", "id")
  temp_SL$id <- stringr::str_extract(basename(temp_SL$id), "[0-9]+")

  # Access the individual data files within the 'rlr_annual' folder
  file_path <- file.path(temp_dir, "rlr_annual", "filelist.txt")
  file_list <- utils::read.csv(file_path, stringsAsFactors = FALSE, header = F, sep = ";")
  colnames(file_list) <- c("id", "Latitude", "Longitude", "name", "coastline", "stationcode", "stationflag")
  # Removing white space in the name of each site
  file_list$name <- stringr::str_trim(file_list$name, side = "both")
  file_list$stationflag <- stringr::str_trim(file_list$stationflag, side = "both")
  # Data from the PSMSL website
  data_TG <- temp_SL %>%
    # pulling out the file number from string so that it matches the name from other files
    dplyr::mutate(id = stringr::str_extract(basename(temp_SL$id), "[0-9]+")) %>%
    # Cases where bad data was collected
    dplyr::filter(!RSL == -99999) %>%
    dplyr::group_by(id) %>%
    # 2000-2018 used as the tidal epoch
    dplyr::mutate(Age_epoch_id = ifelse(dplyr::between(Age, 2000, 2018), TRUE, FALSE))

  # Removing offset based on the location---
  # Offset value is the mean of RSL over the tidal epoch
  # Setting 2000-2018 as the tidal epoch
  Age_epoch_ref <- data_TG %>%
    dplyr::select(RSL, Age_epoch_id) %>%
    dplyr::filter(Age_epoch_id == TRUE) %>%
    dplyr::summarise(RSL_offset = unique(mean(RSL)))

  data_TG <- merge(data_TG, Age_epoch_ref, by = "id", all = TRUE)
  # Cases where no data between 2000-2018 set the offset to 7000
  data_TG$RSL_offset[is.na(data_TG$RSL_offset)] <- 7000

  # Updating the RSL to the shifted RSL value
  data_TG$RSL <- data_TG$RSL - data_TG$RSL_offset

  #--Joining SL data with location names--
  annual_SL_tide_df <- merge(data_TG, file_list, by = "id", all = TRUE)
  #-- Removing sites which have a station flag raised as they are poor sites---
  annual_SL_tide_df <- annual_SL_tide_df %>%
    dplyr::filter(!stationflag == "Y") %>%
    tidyr::drop_na()

  # Remove the temporary file and directory
  unlink(temp_file)
  unlink(temp_dir, recursive = TRUE)

  # Annual Tidal Gauge data----
  annual_tidal_gauge_data_df <- annual_SL_tide_df %>%
    dplyr::select(Age, RSL, Latitude, Longitude, name, RSL_offset, Age_epoch_id) %>%
    dplyr::rename(SiteName = name) %>%
    # from mm --> m
    dplyr::mutate(RSL = RSL / 1000) %>%
    # Reordering by group
    dplyr::group_by(SiteName) %>%
    dplyr::arrange(SiteName, .by_group = TRUE) %>%
    dplyr::arrange(Age) %>%
    dplyr::mutate(data_type_id = "TideGaugeData")


  # # Set the window size for the moving average (in this case, 10 years)
  window_size <- rolling_window_average

  # Create a new column with the rolling average
  annual_tidal_gauge_data_df$rolling_avg <- zoo::rollapply(annual_tidal_gauge_data_df$RSL,
    width = window_size,
    FUN = mean,
    align = "right", # "right",
    fill = NA
  )

  # create a new column for the decade based on the midpoint of the rolling window
  # # annual_tidal_gauge_data_df$decade <- as.integer(floor((annual_tidal_gauge_data_df$Age - (window_size/2))/10)*10)
  # annual_tidal_gauge_data_df$decade <- zoo::rollapply(annual_tidal_gauge_data_df$Age,
  #   width = window_size,
  #   FUN = stats::median,
  #   align = "right", # "right",
  #   fill = NA
  # )
  #
  # calculate the decadal averages based on the rolling average
  decadal_averages_TG <- annual_tidal_gauge_data_df %>% tidyr::drop_na()
  # annual_tidal_gauge_data_df %>%
  # dplyr::group_by(decade, SiteName) %>%
  # dplyr::summarise(
  #   #decade_meanRSL = mean(rolling_avg, na.rm = TRUE),
  #   decade_meanRSL = mean(rolling_avg, na.rm = TRUE),
  #   Age = max(Age),
  #   rows_site = dplyr::n()
  # )
  # plot(decadal_averages_TG$decade,decadal_averages_TG$rolling_avg)


  # Decadal Averages------ I don't know if this is too simple to calculate the decadal averages
  # #offset works better with this
  # decadal_averages_TG <-
  #   annual_tidal_gauge_data_df %>%
  #   dplyr::mutate(decade = (Age - 1) %/% 10) %>%
  #   dplyr::group_by(decade, SiteName) %>%
  #   dplyr::summarise(
  #     #decade_meanRSL = mean(RSL),
  #     rolling_avg = mean(RSL),
  #     Age = max(Age),
  #     rows_site = dplyr::n()
  #   ) # Age=min(Age)

  #---Using standard deviation of RSL over the decade as uncertainty----
  decadal_averages_TG <- decadal_averages_TG %>%
    dplyr::group_by(SiteName) %>%
    # dplyr::mutate(sd_TG = sd(decade_meanRSL))
    dplyr::mutate(sd_TG = sd(rolling_avg))

  #----- New df with decadal averages for tide gauges-----
  tidal_gauge_average_10_df <- merge(decadal_averages_TG, annual_tidal_gauge_data_df)

  #---Rsl & Age error for tidal gauge data----
  tidal_gauge_full_df <- tidal_gauge_average_10_df %>%
    dplyr::mutate(
      Age_err = 5, # years --> half a year/half a decade
    ) %>%
    dplyr::mutate(sd_TG = ifelse(is.na(sd_TG), 0.001, sd_TG)) %>%
    dplyr::group_by(SiteName) %>%
    dplyr::mutate(
      RSL_err = sd_TG
    )

  tidal_gauge_full_df <- tidal_gauge_full_df %>%
    dplyr::mutate(Age = Age / 1000) %>%
    dplyr::mutate(Age_err = Age_err / 1000) %>%
    dplyr::mutate(RSL_annual = RSL) %>%
    dplyr::mutate(RSL = rolling_avg) %>%
    dplyr::select(!c(decade, Age_epoch_id, rolling_avg, RSL_annual, RSL_offset))

  # No user option here -> this is a must: Removing sites with only 2 points (20 years of data)-----
  decadal_TG_df <-
    tidal_gauge_full_df %>%
    # decadal_NA_TG %>%
    dplyr::group_by(SiteName) %>%
    dplyr::filter(dplyr::n() >= 2)
  # dplyr::mutate(data_type_id = "TideGaugeData") %>%

  #-----Uniting original dataset and model run to give a site index to model_result data set-----
  SL_site_df <- data %>%
    dplyr::mutate(Longitude = round(Longitude, 1)) %>%
    dplyr::mutate(Latitude = round(Latitude, 1)) %>%
    tidyr::unite("LongLat", Latitude:Longitude, remove = FALSE) %>% # Uniting 2 columns
    dplyr::mutate(site = sprintf("%02d", as.integer(as.factor(LongLat)))) %>%
    dplyr::mutate(data_type_id = "ProxyRecord") %>%
    dplyr::group_by(SiteName) %>%
    dplyr::mutate(
      Longitude = dplyr::first(Longitude),
      Latitude = dplyr::first(Latitude)
    ) %>%
    dplyr::group_by(SiteName) %>%
    dplyr::mutate(n_obs_by_site = dplyr::n()) %>%
    dplyr::ungroup()

  SL_tide_site_df <- decadal_TG_df %>%
    # dplyr::select(!all_tidal_data_sites) %>%
    dplyr::mutate(Longitude = round(Longitude, 1)) %>%
    dplyr::mutate(Latitude = round(Latitude, 1)) %>%
    tidyr::unite("LongLat", Latitude:Longitude, remove = FALSE) %>% # Uniting 2 columns
    dplyr::mutate(site = sprintf("%02d", as.integer(as.factor(LongLat)))) %>%
    dplyr::mutate(data_type_id = "TideGaugeData") %>%
    dplyr::group_by(SiteName) %>%
    dplyr::mutate(n_obs_by_site = dplyr::n()) %>%
    dplyr::ungroup()

  #------Joining proxy sites to gauges based on shortest distance----
  SL_proxy_unique <- SL_site_df %>%
    dplyr::select(SiteName, Longitude, Latitude, data_type_id, n_obs_by_site) %>%
    unique() %>%
    as.data.frame()
  SL_tide_unique <- SL_tide_site_df %>%
    dplyr::select(SiteName, Longitude, Latitude, data_type_id, n_obs_by_site) %>%
    unique() %>%
    as.data.frame()

  #---Distance Matrix for each site to each other---
  mat.distance <- geosphere::distm(SL_proxy_unique[, 2:3], SL_tide_unique[, 2:3])
  # fun = distGeo)
  mat.distance_m <- as.matrix(mat.distance)
  #--finding row mins & corresponding tidal gauge--
  rownames(mat.distance) <- SL_proxy_unique$SiteName
  colnames(mat.distance) <- SL_tide_unique$SiteName
  #--finding row mins & corresponding tidal gauge--
  dist_TG_proxy <- t(sapply(seq(nrow(mat.distance)), function(z) {
    js <- order((mat.distance[z, ]))[1:5]
    c(
      rownames(mat.distance)[z], colnames(mat.distance)[js[1]], mat.distance[z, js[1]],
      colnames(mat.distance)[js[2]], mat.distance[z, js[2]],
      colnames(mat.distance)[js[3]], mat.distance[z, js[3]],
      colnames(mat.distance)[js[4]], mat.distance[z, js[4]],
      colnames(mat.distance)[js[5]], mat.distance[z, js[5]]
    )
  }))

  dist_TG_proxy <- as.data.frame(dist_TG_proxy)
  colnames(dist_TG_proxy) <- c(
    "nearest_proxy_site",
    "TG_site_1", "TG_min_dist1",
    "TG_site_2", "TG_min_dist2",
    "TG_site_3", "TG_min_dist3",
    "TG_site_4", "TG_min_dist4",
    "TG_site_5", "TG_min_dist5"
  )
  # Sorting the minimum distances from lowest to highest
  dist_TG_proxy <- dist_TG_proxy %>% dplyr::arrange(dplyr::desc(TG_min_dist1))

  dist_TG_proxy_long_1 <- dist_TG_proxy %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with(c("TG_min_dist")),
      values_to = c("minimum_distance")
    )
  dist_TG_proxy_long_2 <- dist_TG_proxy %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with(c("TG_site")),
      values_to = c("nearest_TG")
    )
  obs_sites <- SL_tide_unique %>%
    dplyr::filter(SiteName %in% dist_TG_proxy_long_2$nearest_TG) %>%
    dplyr::select(n_obs_by_site)
  dist_TG_proxy_df_new <- data.frame(
    nearest_proxy_site = dist_TG_proxy_long_1$nearest_proxy_site,
    nearest_TG = dist_TG_proxy_long_2$nearest_TG,
    minimum_dist = as.numeric(dist_TG_proxy_long_1$minimum_distance)
  ) # ,
  # n_obs_tg = obs_sites)


  # Criteria 1: User provides a list of TGs------------------------
  if (is.null(list_preferred_TGs) == FALSE) {
    # Check if TG exists in the list
    check_TG <- all(list_preferred_TGs %in% unique(decadal_TG_df$SiteName))
    if (check_TG == FALSE) {
      cat("Warning: Tide Gauge provided does not exist or may contain a misprint in the name.\n")
      stop()
    }

    decadal_TG_df_filter <- subset(decadal_TG_df, SiteName %in% list_preferred_TGs)
    #--There will be NAs were the proxy data doesn't have a corresponding index--
    data_tide_proxy <- plyr::rbind.fill(
      SL_site_df,
      decadal_TG_df_filter
    )

    # Ensuring the SiteName is a factor
    data <- data_tide_proxy %>%
      dplyr::select(!c(
        n_obs_by_site, site, sd_TG
      )) %>%
      dplyr::mutate(
        SiteName = as.factor(SiteName),
        data_type_id = as.factor(data_type_id)
      )
    message("Choosing the tide gauges that the user listed as their preferred tide gauges. \n")
  }

  # Criteria 2: Minimum distance to proxy site
  if (TG_minimum_dist_proxy == TRUE) {
    # Finding the closest TG
    all_nearest_TG_closest <- dist_TG_proxy_df_new %>%
      dplyr::group_by(nearest_proxy_site) %>%
      dplyr::filter(minimum_dist == min(minimum_dist)) %>%
      dplyr::distinct(nearest_TG, .keep_all = TRUE) # Removing any duplicate tide gauge sites.


    # Joining the selected TG sites back with the original data
    join_new_index_tide_df <- SL_tide_site_df %>%
      dplyr::filter(SiteName %in% all_nearest_TG_closest$nearest_TG)

    #--There will be NAs were the proxy data doesn't have a corresponding index--
    data_tide_proxy <- plyr::rbind.fill(
      SL_site_df,
      join_new_index_tide_df
    ) # stacking rows


    # Ensuring the SiteName is a factor
    data <- data_tide_proxy %>%
      dplyr::select(!c(
        # RSL_annual, Age_epoch_id,
        # RSL_offset,  rows_site,
        # decade_meanRSL,#rolling_avg,
        n_obs_by_site, site, sd_TG
        # Indicator,Basin,
      )) %>%
      dplyr::mutate(
        SiteName = as.factor(SiteName),
        data_type_id = as.factor(data_type_id)
      )
    message("Selecting the tide gauge with the minimum distance to the proxy site \n")
  }
  # Criteria 3: All tide gauges within 1 degree away from proxy site
  if (all_TG_1deg == TRUE) {
    # 1 degree away from proxy site is 111.1km
    all_nearest_TG_closest <- dist_TG_proxy_df_new %>%
      dplyr::filter(minimum_dist <= 111100) %>%
      dplyr::distinct(nearest_TG, .keep_all = TRUE) # Removing any duplicate tide gauge sites.

    # Joining the selected TG sites back with the original data
    join_new_index_tide_df <- SL_tide_site_df %>%
      dplyr::filter(SiteName %in% all_nearest_TG_closest$nearest_TG)

    #--There will be NAs were the proxy data doesn't have a corresponding index--
    data_tide_proxy <- plyr::rbind.fill(
      SL_site_df,
      join_new_index_tide_df
    ) # stacking rows


    # Ensuring the SiteName is a factor
    data <- data_tide_proxy %>%
      dplyr::select(!c(
        # RSL_annual, Age_epoch_id,
        # RSL_offset, sd_TG, rows_site,
        # decade_meanRSL,#rolling_avg,
        n_obs_by_site, site, sd_TG
        # Indicator,Basin,
      )) %>%
      dplyr::mutate(
        SiteName = as.factor(SiteName),
        data_type_id = as.factor(data_type_id)
      )
    message("Selecting all tide gauges within 1 degree of the proxy site \n")
  }

  # Criteria 4: All tide gauges within 1 degree away from proxy site & the preferred tide gauges listed by user
  if (TG_minimum_dist_proxy == TRUE  & is.null(list_preferred_TGs) == FALSE) {
    # Check if TG exists in the list
    check_TG <- all(list_preferred_TGs %in% unique(decadal_TG_df$SiteName))
    if (check_TG == FALSE) {
      cat("Warning: Tide Gauge provided does not exist or may contain a misprint in the name.\n")
      stop()
    }
    decadal_TG_df_filter <- subset(decadal_TG_df, SiteName %in% list_preferred_TGs)
    #--There will be NAs were the proxy data doesn't have a corresponding index--
    data_tide_proxy_TGlist <- plyr::rbind.fill(
      SL_site_df,
      decadal_TG_df_filter
    ) # stacking rows

    # 1 degree away from proxy site is 111.1km-------------
    all_nearest_TG_closest <- dist_TG_proxy_df_new %>%
      dplyr::filter(minimum_dist <= 111100) %>%
      dplyr::distinct(nearest_TG, .keep_all = TRUE) # Removing any duplicate tide gauge sites.

    # Joining the selected TG sites back with the original data
    join_new_index_tide_df <- SL_tide_site_df %>%
      dplyr::filter(SiteName %in% all_nearest_TG_closest$nearest_TG)

    #--There will be NAs were the proxy data doesn't have a corresponding index--
    data_tide_proxy_all_1deg <- plyr::rbind.fill(
      SL_site_df,
      join_new_index_tide_df
    )

    # Combining both dataframes & removing duplicates----
    data_tide_proxy <- merge(data_tide_proxy_all_1deg,data_tide_proxy_TGlist,all = TRUE)
    data <- data_tide_proxy %>%
      dplyr::select(!c(
        n_obs_by_site, site, sd_TG
      )) %>%
      dplyr::mutate(
        SiteName = as.factor(SiteName),
        data_type_id = as.factor(data_type_id)
      )
  }
  # Criteria 5: Closest tide gauges to the proxy site & the preferred tide gauges listed by user
  if (all_TG_1deg == TRUE  & is.null(list_preferred_TGs) == FALSE) {
    # Check if TG exists in the list
    check_TG <- all(list_preferred_TGs %in% unique(decadal_TG_df$SiteName))
    if (check_TG == FALSE) {
      cat("Warning: Tide Gauge provided does not exist or may contain a misprint in the name.\n")
      stop()
    }
    decadal_TG_df_filter <- subset(decadal_TG_df, SiteName %in% list_preferred_TGs)
    #--There will be NAs were the proxy data doesn't have a corresponding index--
    data_tide_proxy_TGlist <- plyr::rbind.fill(
      SL_site_df,
      decadal_TG_df_filter
    ) # stacking rows

    # Finding the closest TG
    all_nearest_TG_closest <- dist_TG_proxy_df_new %>%
      dplyr::group_by(nearest_proxy_site) %>%
      dplyr::filter(minimum_dist == min(minimum_dist)) %>%
      dplyr::distinct(nearest_TG, .keep_all = TRUE) # Removing any duplicate tide gauge sites.


    # Joining the selected TG sites back with the original data
    join_new_index_tide_df <- SL_tide_site_df %>%
      dplyr::filter(SiteName %in% all_nearest_TG_closest$nearest_TG)

    #--There will be NAs were the proxy data doesn't have a corresponding index--
    data_tide_proxy_closestTG <- plyr::rbind.fill(
      SL_site_df,
      join_new_index_tide_df
    ) # stacking rows


    # Combining both dataframes & removing duplicates----
    data_tide_proxy <- merge(data_tide_proxy_all_1deg,data_tide_proxy_closestTG,all = TRUE)
    # Ensuring the SiteName is a factor
    data <- data_tide_proxy %>%
      dplyr::select(!c(
        n_obs_by_site, site, sd_TG
      )) %>%
      dplyr::mutate(
        SiteName = as.factor(SiteName),
        data_type_id = as.factor(data_type_id)
      )
  }

  return(data)
}

#' If the user decides to include tide gauge data, this function adds the linear rate and the associated linear rate error for those sites.
#' The linear rate comes from a physical model known as an Earth-ice models which use a representation of the physical Earth structure (such as lithospheric thickness and properties such as mantle viscosity) topredict changes in GIA that occur through loading and unloading of ice, and provide estimates of GIA rates.
#' One such example of an Earth-ice physical model is the ICE5G VM2-90 created by Peltier, 2004 and is used as the source of the linear rates for the tide gauges.
#' Engelhart et al 2009 demonstrated the associated uncertainty for the linear rate for tide gauges to be 0.3mm per year.
#' Hence, this function calculates these rates and uncertainty values for tide gauge data.
#'
#' @param data Input data
#' @noRd
add_linear_rate <- function(data) {
  # GIA DATA from Peltier Website ICE5G----------------
  # Set up the URL for downloading the data
  url <- "https://www.atmosp.physics.utoronto.ca/~peltier/datasets/GRID/dsea250.1grid.ICE5Gv1.3_VM2_L90_2012.nc"
  # Create a temporary file
  temp_file <- tempfile()

  # Download the file and save it to the temporary file
  utils::download.file(url,
    destfile = temp_file, # "dsea250.1grid.ICE5Gv1.3_VM2_L90_2012.nc",#temp_file,
    method = "libcurl", mode = "wb"
  )
  # Unzip the data file to a temporary directory
  temp_dir <- tempfile()
  suppressWarnings(
    utils::unzip(temp_file, exdir = temp_dir)
  )
  # Opening the files
  nc_data <- ncdf4::nc_open(temp_file) # ICE5G: better fit for data

  # Rounding to 1 decimal point to reduce number of spatial options--
  dat_lon <- round(data$Longitude, 1)
  dat_lat <- round(data$Latitude, 1)

  # Get lon and lat from GIA model output
  lon <- round(ncdf4::ncvar_get(nc_data, "Lon"), 1)
  lat <- round(ncdf4::ncvar_get(nc_data, "Lat"), 1)

  # Needs to be sorted for the match.closest function() below
  # Note need the index for the unsorted coordinate for pulling the correct SL rate later
  gia_lat <- dplyr::tibble(index = 1:length(lat), lat) %>% dplyr::arrange(lat)
  gia_lon <- dplyr::tibble(index = 1:length(lon), lon) %>% dplyr::arrange(lon)

  # Matching closest long & lat values
  lat_index <- gia_lat$index[match.closest(dat_lat, gia_lat$lat)]
  lon_index <- gia_lon$index[match.closest(360 + dat_lon, gia_lon$lon)] # change data lon to degrees east

  # GIA rates
  SL <- ncdf4::ncvar_get(nc_data, "Dsea_250")
  # Repicating to match dim of data
  linear_slope <- rep(NA, nrow(data))
  for (i in 1:nrow(data)) {
    linear_slope[i] <- (SL[lon_index[i], lat_index[i]])
  }

  # Combining linear with other dataset
  data <- cbind(data, ICE5_GIA_slope = linear_slope) # mm/yr

  # Remove the temporary file and directory
  unlink(temp_file)
  unlink(temp_dir, recursive = TRUE)

  return(data)
}

#' Match.closest: Will be used when matching similar long & lat values from both datasets
#'
#' @param x Input data
#' @param table Table of options
#' @param tolerance Identifying the nearby sites
#' @param nomatch No match
#' @noRd
match.closest <- function(x, table, tolerance = Inf, nomatch = NA_integer_) {
  #------Match Closest function doesn't exist on version R 3.6.3----
  #--Will be used when matching similar long & lat values from both datasets--
  lIdx <- findInterval(x, table, rightmost.closed = FALSE, all.inside = TRUE)
  rIdx <- lIdx + 1L
  lIdx[lIdx == 0L] <- 1L
  lDiff <- abs(table[lIdx] - x)
  rDiff <- abs(table[rIdx] - x)
  d <- which(lDiff >= rDiff)
  lIdx[d] <- rIdx[d]
  if (any(is.finite(tolerance))) {
    if (any(tolerance < 0L)) {
      warning(sQuote("tolerance"), " < 0 is meaningless. Set to zero.")
      tolerance[tolerance < 0L] <- 0L
    }
    if (length(nomatch) != 1L) {
      stop("Length of ", sQuote("nomatch"), " has to be one.")
    }
    tolerance <- rep_len(tolerance, length(table))
    lDiff[d] <- rDiff[d]
    lIdx[lDiff > tolerance[lIdx]] <- nomatch
  }
  lIdx
}

#' Linear rate estimated using the data
#'
#' @param data Input data
#' @noRd
linear_reg_rates <- function(data) {
  Age <- RSL <- Age_err <- RSL_err <- linear_rate <- linear_rate_err <- SiteName <- Longitude <- Latitude <- NULL
  data_filter <- data %>%
    dplyr::filter(!Age > 1.800) # Ignoring recent human influences to SL rise

  # Doing linear regression on rest of data
  data_lm <- data_filter %>%
    dplyr::group_by(SiteName) %>%
    dplyr::mutate(
      linear_rate = round(stats::lm(RSL ~ Age)$coefficients[[2]], 2),
      linear_rate_err = base::summary(stats::lm(RSL ~ Age))$coefficients[2, 2]
    )


  # Table of GIA rate vs lm rate from proxy data
  lm_slopes <- data_lm %>%
    dplyr::select(SiteName, linear_rate, linear_rate_err) %>%
    unique()
  return(lm_slopes)
}

#' Function to create dataframe for plotting using IGP results
#'
#' @param model_run_output  The JAGS output
#' @param jags_data Data associated with IGP data
#' @param data_grid Input data grid
#' @param CI Size of the credible interval, default is 95% and the user can use "50%","95%" or "99%"
#' @noRd
create_igp_output_df <- function(model_run, jags_data, data_grid,CI) {
  m <- model_run$BUGSoutput$sims.matrix
  sample_draws <- tidybayes::tidy_draws(m)
  n_iter <- sample_draws$.iteration %>%
    unique() %>%
    length()
  # If the user sets iteration value extremely high and to save time reduce it
  if (model_run$n.iter > 10000) {
    sample_draws <- sample_draws %>% dplyr::slice_sample(n = 1000)
    n_iterations <- 1000
  }
  jags_data <- jags_data
  # Get predictions on a grid of t values.
  Ngrid <- jags_data$Ngrid
  tgrid <- jags_data$tstar
  tstar <- jags_data$tstar
  Dist <- jags_data$Dist

  # Set up the matrix that will contain the estimates
  pred_full <- matrix(NA, ncol = Ngrid, nrow = n_iter)
  K.gw <- K <- K.w.inv <- array(NA, c(n_iter, Ngrid, Ngrid))

  ######## Initialize quadrature for the integration########
  # DOES this need to change if I change the size of the prediction grids?
  L <- 30 ## this sets the precision of the integration quadrature (higher is better but more computationally expensive)
  index <- 1:L
  cosfunc <- cos(((2 * index - 1) * pi) / (2 * L))

  quad1 <- array(dim = c(nrow = Ngrid, ncol = Ngrid, L))
  quad2 <- array(dim = c(nrow = Ngrid, ncol = Ngrid, L))

  for (j in 1:Ngrid) {
    for (k in 1:Ngrid) {
      quad1[k, j, ] <- abs((tgrid[k] * cosfunc / 2) + (tgrid[k] / 2) - tstar[j])^1.99
      quad2[k, j, ] <- ((tgrid[k] / 2) * (pi / L)) * (sqrt(1 - cosfunc^2))
    }
  }


  # Get posterior samples of rates
  w.ms <- as.matrix(model_run$BUGSoutput$sims.list$w.m)

  # Get estimates
  for (i in 1:n_iter) {
    for (k in 1:Ngrid) {
      for (j in 1:Ngrid) {
        K.gw[i, j, k] <- sum((sample_draws$phi[i]^quad1[j, k, ]) * quad2[j, k, ]) #### Quadrature function
      } # End j loop
    } # End k loop

    K[i, , ] <- sample_draws$phi[i]^(Dist^1.99)
    K.w.inv[i, , ] <- solve(K[i, , ])
    pred_full[i, ] <- sample_draws$alpha[i] + K.gw[i, , ] %*% K.w.inv[i, , ] %*% w.ms[i, ]
  } # End i loop
  # pred_full <- pred_full * mod$scale_factor_y
  # w.ms <- (w.ms * mod$scale_factor_y) / mod$scale_factor_x

  if(CI =="99%"){
    upr = apply(pred_full, 2, stats::quantile, probs = 0.005)
    lwr = apply(pred_full, 2, stats::quantile, probs = 0.995)
    rate_lwr = apply(w.ms, 2, stats::quantile, probs = 0.005)
    rate_upr = apply(w.ms, 2, stats::quantile, probs = 0.995)
  }

  if(CI =="95%"){
    upr = apply(pred_full, 2, stats::quantile, probs = 0.025)
    lwr = apply(pred_full, 2, stats::quantile, probs = 0.975)
    rate_lwr = apply(w.ms, 2, stats::quantile, probs = 0.025)
    rate_upr = apply(w.ms, 2, stats::quantile, probs = 0.975)

  }
  if(CI == "50%"){
    upr = apply(pred_full, 2, stats::quantile, probs = 0.25)
    lwr = apply(pred_full, 2, stats::quantile, probs = 0.75)
    rate_upr = apply(w.ms, 2, stats::quantile, probs = 0.25)
    rate_lwr = apply(w.ms, 2, stats::quantile, probs = 0.75)
  }

  # Output dataframes for plots
  output_dataframes <- dplyr::tibble(
    data_grid,
    pred = apply(pred_full, 2, mean),
    upr = upr,
    lwr = lwr,
    #lwr_95 = apply(pred_full, 2, stats::quantile, probs = 0.025),
    #upr_95 = apply(pred_full, 2, stats::quantile, probs = 0.975),
    #upr_50 = apply(pred_full, 2, stats::quantile, probs = 0.25),
    #lwr_50 = apply(pred_full, 2, stats::quantile, probs = 0.75),
    rate_pred = apply(w.ms, 2, mean),
    rate_upr = rate_upr,
    rate_lwr = rate_lwr,
    CI = CI
    #rate_lwr_95 = apply(w.ms, 2, stats::quantile, probs = 0.025),
    #rate_upr_95 = apply(w.ms, 2, stats::quantile, probs = 0.975),
    #rate_upr_50 = apply(w.ms, 2, stats::quantile, probs = 0.25),
    #rate_lwr_50 = apply(w.ms, 2, stats::quantile, probs = 0.75)
  )
  return(output_dataframes)
}


#' Function to create the dataframes for plotting
#'
#' @param noisy_model_run_output The JAGS output
#' @param rate_grid If rate of change is included in the dataframe
#' @param decomposition Is the full model decomposition included in dataframe
#' @param CI Size of the credible intervals. The default in "95%" and the user can decide "50%", "95%" or "90%"
#' @noRd
create_output_df <- function(noisy_model_run_output,
                             data_grid, # jags_output,
                             rate_grid = FALSE,
                             decomposition = FALSE,
                             CI) {
  ID <- NULL
  if (rate_grid == TRUE ) {

    mu_post_pred <- noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    #mu_post_pred <- noisy_model_run_output$BUGSoutput$sims.list$mu_y
    mu_pred_deriv_post <- noisy_model_run_output$BUGSoutput$sims.list$mu_pred_deriv
    #mu_pred_deriv_post <- noisy_model_run_output$BUGSoutput$sims.list$mu_deriv

    if(CI =="99%"){
      upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.005)
      lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.995)
      rate_upr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.005)
      rate_lwr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.995)
    }
    if(CI =="95%"){
      upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025)
      lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975)
      rate_upr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.025)
      rate_lwr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.975)

    }
    if(CI == "50%"){
      upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.25)
      lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.75)
      rate_upr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.25)
      rate_lwr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.75)

    }

    output_dataframes <- data.frame(
      data_grid,
      pred = apply(mu_post_pred, 2, mean),
      upr = upr,
      lwr = lwr,
      rate_pred =  apply(mu_pred_deriv_post, 2, mean),
      rate_upr = rate_upr,
      rate_lwr = rate_lwr,
      CI = CI
      #upr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      #lwr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      #upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      #lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75)
    )

    # output_dataframes <- data.frame(
    #   output_dataframes,
    #   rate_pred =  apply(mu_pred_deriv_post, 2, mean),
    #   rate_upr = rate_upr,
    #   rate_lwr = rate_lwr
    #   #rate_upr_95 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.025),
    #   #rate_lwr_95 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.975),
    #   #rate_upr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.25),
    #   #rate_lwr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.75)
    # )
  } else {

    mu_post_pred <- noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    if(CI =="99%"){
      upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.005)
      lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.995)
    }
    if(CI =="95%"){
      upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025)
      lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975)
    }
    if(CI == "50%"){
      upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.25)
      lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.75)
    }
    output_dataframes <- data.frame(
      data_grid,
      pred = apply(mu_post_pred, 2, mean),
      upr = upr,
      lwr = lwr,
      CI = CI
      #upr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      #lwr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      #upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      #lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75)
    )
    output_dataframes <- output_dataframes
  }

  if (decomposition == TRUE & rate_grid == TRUE) {
    # Total Component from JAGS output
    mu_post_pred <- noisy_model_run_output$BUGSoutput$sims.list$mu_pred
    # mu_post_pred <- noisy_model_run_output$BUGSoutput$sims.list$mu_y
    if(CI =="99%"){
      upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.005)
      lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.995)
    }

    if(CI =="95%"){
      upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025)
      lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975)
    }
    if(CI == "50%"){
      upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.25)
      lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.75)
    }
    total_model_fit_df <- data.frame(
      data_grid,
      pred = apply(mu_post_pred, 2, mean),
      upr = upr,
      lwr = lwr,
      #upr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
      #lwr_95 = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
      #upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
      #lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
      ID = "Total Posterior Model",
      CI = CI
    )

    # Total model fit rate
    mu_pred_deriv_post <- noisy_model_run_output$BUGSoutput$sims.list$mu_pred_deriv
    # mu_pred_deriv_post <- noisy_model_run_output$BUGSoutput$sims.list$mu_deriv
    if(CI =="99%"){
      rate_upr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.005)
      rate_lwr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.995)
    }
    if(CI =="95%"){
      rate_upr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.025)
      rate_lwr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.975)
    }
    if(CI == "50%"){
      rate_upr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.25)
      rate_lwr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.75)
    }
    total_model_rate_df <-
      data.frame(
        data_grid,
        rate_pred = apply(mu_pred_deriv_post, 2, mean),
        rate_upr = rate_upr,
        rate_lwr = rate_lwr,
        #rate_upr_95 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.025),
        #rate_lwr_95 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.975),
        #rate_upr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.25),
        #rate_lwr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.75),
        ID = "Total Rate of Change for Posterior Model",
        CI = CI
      )

    # Regional component
    time_component_pred_post <- noisy_model_run_output$BUGSoutput$sims.list$r_pred
    # time_component_pred_post <- noisy_model_run_output$BUGSoutput$sims.list$r
    if(CI =="99%"){
      upr = apply(time_component_pred_post, 2, stats::quantile, probs = 0.005)
      lwr = apply(time_component_pred_post, 2, stats::quantile, probs = 0.995)
    }
    if(CI =="95%"){
      upr = apply(time_component_pred_post, 2, stats::quantile, probs = 0.025)
      lwr = apply(time_component_pred_post, 2, stats::quantile, probs = 0.975)
    }
    if(CI == "50%"){
      upr = apply(time_component_pred_post, 2, stats::quantile, probs = 0.25)
      lwr = apply(time_component_pred_post, 2, stats::quantile, probs = 0.75)
    }
    regional_component_df <- data.frame(
      data_grid,
      pred = apply(time_component_pred_post, 2, mean),
      upr = upr,
      lwr = lwr,
      #upr_95 = apply(time_component_pred_post, 2, stats::quantile, probs = 0.025),
      #lwr_95 = apply(time_component_pred_post, 2, stats::quantile, probs = 0.975),
      #upr_50 = apply(time_component_pred_post, 2, stats::quantile, probs = 0.25),
      #lwr_50 = apply(time_component_pred_post, 2, stats::quantile, probs = 0.75),
      ID = "Regional Component",
      CI = CI
    )

    time_component_pred_deriv_post <- noisy_model_run_output$BUGSoutput$sims.list$r_pred_deriv
    # time_component_pred_deriv_post <- noisy_model_run_output$BUGSoutput$sims.list$r_deriv
    if(CI =="99%"){
      rate_upr = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.005)
      rate_lwr = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.995)
    }
    if(CI =="95%"){
      rate_upr = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.025)
      rate_lwr = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.975)
    }
    if(CI == "50%"){
      rate_upr = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.25)
      rate_lwr = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.75)
    }
    regional_rate_component_df <-
      data.frame(
        data_grid,
        rate_pred = apply(time_component_pred_deriv_post, 2, mean),
        rate_upr = rate_upr,
        rate_lwr = rate_lwr,
        #rate_upr_95 = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.025),
        #rate_lwr_95 = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.975),
        #rate_upr_50 = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.25),
        #rate_lwr_50 = apply(time_component_pred_deriv_post, 2, stats::quantile, probs = 0.75),
        ID = "Rate of Change for Regional Component",
        CI = CI
      )

    # Vertical Offset & Linear Local Component
    g_h_component_pred_post <- noisy_model_run_output$BUGSoutput$sims.list$g_h_z_x_pred
    # g_h_component_pred_post <- noisy_model_run_output$BUGSoutput$sims.list$g_h_z_x
    if(CI =="99%"){
      upr = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.005)
      lwr = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.995)
    }
    if(CI =="95%"){
      upr = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.025)
      lwr = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.975)
    }
    if(CI == "50%"){
      upr = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.25)
      lwr = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.75)
    }
    lin_loc_component_df <-
      data.frame(
        data_grid,
        pred = apply(g_h_component_pred_post, 2, mean),
        upr = upr,
        lwr = lwr,
        #upr_95 = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.025),
        #lwr_95 = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.975),
        #upr_50 = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.25),
        #lwr_50 = apply(g_h_component_pred_post, 2, stats::quantile, probs = 0.75),
        ID = "Site Specific vertical offset + \n Linear Local Component",
        CI = CI
      )

    # Non linear local component
    space_time_component_pred_post <- noisy_model_run_output$BUGSoutput$sims.list$l_pred
    # space_time_component_pred_post <- noisy_model_run_output$BUGSoutput$sims.list$l
    if(CI =="99%"){
      upr = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.005)
      lwr = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.995)
    }
    if(CI =="95%"){
      upr = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.025)
      lwr = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.975)
    }
    if(CI == "50%"){
      upr = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.25)
      lwr = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.75)
    }
    non_lin_loc_component_df <-
      data.frame(
        data_grid,
        pred = apply(space_time_component_pred_post, 2, mean),
        upr = upr,
        lwr = lwr,
        #upr_95 = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.025),
        #lwr_95 = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.975),
        #upr_50 = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.25),
        #lwr_50 = apply(space_time_component_pred_post, 2, stats::quantile, probs = 0.75),
        ID = "Non Linear Local Component",
        CI = CI
      )
    space_time_component_pred_deriv_post <- noisy_model_run_output$BUGSoutput$sims.list$l_pred_deriv
    # space_time_component_pred_deriv_post <- noisy_model_run_output$BUGSoutput$sims.list$l_deriv
    if(CI =="99%"){
      rate_upr = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.005)
      rate_lwr = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.995)
    }
    if(CI =="95%"){
      rate_upr = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.025)
      rate_lwr = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.975)
    }
    if(CI == "50%"){
      rate_upr = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.25)
      rate_lwr = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.75)
    }
    non_lin_loc_rate_component_df <-
      data.frame(
        data_grid,
        rate_pred = apply(space_time_component_pred_deriv_post, 2, mean),
        rate_upr = rate_upr,
        rate_lwr = rate_lwr,
        #rate_upr_95 = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.025),
        #rate_lwr_95 = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.975),
        #rate_upr_50 = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.25),
        #rate_lwr_50 = apply(space_time_component_pred_deriv_post, 2, stats::quantile, probs = 0.75),
        ID = "Rate of Change for Non Linear Local Component",
        CI = CI
      )


    output_dataframes <- list(
      total_model_fit_df = total_model_fit_df %>% dplyr::mutate(ID = as.factor(ID)),
      total_model_rate_df = total_model_rate_df %>% dplyr::mutate(ID = as.factor(ID)),
      regional_component_df = regional_component_df %>% dplyr::mutate(ID = as.factor(ID)),
      regional_rate_component_df = regional_rate_component_df %>% dplyr::mutate(ID = as.factor(ID)),
      lin_loc_component_df = lin_loc_component_df %>% dplyr::mutate(ID = as.factor(ID)),
      non_lin_loc_component_df = non_lin_loc_component_df%>% dplyr::mutate(ID = as.factor(ID)),
      non_lin_loc_rate_component_df = non_lin_loc_rate_component_df%>% dplyr::mutate(ID = as.factor(ID))
    )
  }

  return(output_dataframes)
}
#' Adding Noisy Input to the dataframe
#'
#' @param model_run JAGS output
#' @param model_type NIGAM in time or space time or the full decomposition
#' @param data Input data
#' @param nseg Number of segments used to create basis functions for splines
#' @param spline_nseg_t Number of segments used to create basis functions for spline in time
#' @param spline_nseg_st Number of segments used to create basis functions for spline in space time
#' @noRd
add_noisy_input <- function(model_run, jags_data, model_type,
                            data,
                            spline_nseg_t,
                            spline_nseg_st#,
                            #xr,
                            #xl
                            ) {
  if (model_type == "ni_spline_t") {
    #-----Get posterior samples for SL-----
    b_t_post <- model_run$BUGSoutput$sims.list$b_t

    pred_mean_calc <- function(t_new) {
      # Create the basis functions
      #B_deriv_t <- predict(B_t, t_new)

      B_deriv_t <- bs_bbase_t(t_new,
                              #xl = xl,
                              #xr=xr,
        xl = min(data$Age),
        xr = max(data$Age),
        data = data,
        spline_nseg_t = spline_nseg_t)

      #----Deriv----
      return(B_deriv_t %*% colMeans(b_t_post))
    }
    #-------Now create derivatives----
    h <- 0.00001
    t <- data$Age
    deriv <- (pred_mean_calc(t + h) - pred_mean_calc(t - h)) / (2 * h)
  }

  if (model_type == "ni_spline_st") {
    b_st_post <- model_run$BUGSoutput$sims.list$b_st

    pred_mean_calc <- function(t_new) {
      B_time <- bs_bbase_st(t_new,
        xl = min(data$Age),
        xr = max(data$Age),
        data = data,
        spline_nseg_st = spline_nseg_st
      )
      B_space_1 <- bs_bbase_st(data$Latitude,
        xl = min(data$Latitude),
        xr = max(data$Latitude),
        data = data,
        spline_nseg_st = spline_nseg_st
      )
      B_space_2 <- bs_bbase_st(data$Longitude,
        xl = min(data$Longitude),
        xr = max(data$Longitude),
        data = data
      )
      B_l_deriv_full <- matrix(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data)
      )
      regional_knots_loc <- rep(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1)
      )
      count <- 1
      for (i in 1:ncol(B_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_l_deriv_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero
      B_l_deriv <- B_l_deriv_full[, -which(colSums(B_l_deriv_full) < 0.1)]
      return(B_l_deriv %*% colMeans(b_st_post))
    }
    #-------Now create derivatives----
    h <- 0.0001
    t <- data$Age
    deriv <- (pred_mean_calc(t + h) - pred_mean_calc(t - h)) / (2 * h)
  }

  if (model_type == "ni_gam_decomp") {
    #-----Get posterior samples for SL-----
    intercept_post <- model_run$BUGSoutput$sims.list$intercept
    b_t_post <- model_run$BUGSoutput$sims.list$b_t
    b_g_post <- model_run$BUGSoutput$sims.list$b_g

    pred_mean_calc <- function(t_new) {
      # Create the regional basis functions
      B_t <- bs_bbase_t(t_new,
        xl = min(data$Age),
        xr = max(data$Age),
        spline_nseg_t = spline_nseg_t,
        data = data
      )
      #----Deriv----
      return(intercept_post[data$SiteName] + B_t %*% colMeans(b_t_post) + b_g_post[data$SiteName] * (t_new))
    }
    #-------Now create derivatives----
    h <- 0.01
    t <- data$Age
    deriv <- (pred_mean_calc(t + h) - pred_mean_calc(t - h)) / (2 * h)
  }

  # Add this new term in - this is the extra standard deviation on each term----
  data$NI_var_term <- sqrt(deriv^2 %*% data$Age_err^2)[, 1]

  # Writing new dataframe with noisy extra column------
  data <- data.frame(data)
  return(data)
}


#' Correcting data from EIV-IGP model
#'
#' @param data Input data
#' @noRd

igp_data <- function(data, data_grid) {
  Age <- RSL <- Longitude <- Latitude <- SiteName <- NULL
  ############# Set up the grid for the GP ###################
  tgrid <- data_grid$Age
  Ngrid <- length(tgrid)

  ### Change data to lower zero for integration
  min_t <- min(data$Age)
  t <- data$Age - min_t
  tstar <- tgrid - min_t

  Dist <- fields::rdist(tstar) ### Distance matrix required for the model
  D <- cbind(t, data$RSL) ### Combine the x,y data for the model

  ######## Initialize quadrature for the integration########
  N <- nrow(data)
  L <- 30 ## this sets the precision of the integration quadrature (higher is better but more computationally expensive)
  index <- 1:L
  cosfunc <- cos(((2 * index - 1) * pi) / (2 * L))

  quad1 <- array(dim = c(nrow = N, ncol = Ngrid, L))
  quad2 <- array(dim = c(nrow = N, ncol = Ngrid, L))

  for (j in 1:Ngrid)
  {
    for (k in 1:N)
    {
      quad1[k, j, ] <- abs((t[k] * cosfunc / 2) + (t[k] / 2) - tstar[j])^1.99
      quad2[k, j, ] <- ((t[k] / 2) * (pi / L)) * (sqrt(1 - cosfunc^2))
    }
  }


  return(list(
    tstar = tstar,
    N = N,
    Ngrid = Ngrid,
    Dist = Dist,
    quad1 = quad1,
    quad2 = quad2,
    cosfunc = cosfunc,
    ppi = pi,
    L = L
  ))
}

#' Creating basis function for splines
#'
#' @param data Input data
#' @param data_grid Prediction data
#' @param model_type Type of model
#' @param spline_nseg_t Number of segments for the creation of the basis functions for spline in time
#' @param spline_nseg_st Number of segments for the creation of the basis functions for spline in space time
#' @noRd


spline_basis_fun <- function(data,
                             data_grid,
                             model_type,
                             spline_nseg_st,
                             spline_nseg_t#,
                             #xl,
                             #xr
                             ) {
  Age <- RSL <- Longitude <- Latitude <- SiteName <- NULL

  if (model_type == "ni_spline_t") {
    t <- data$Age
    # Basis functions in time for data-----------------------
    B_t <- bs_bbase_t(t,
                    xl = min(t),
                    #xl = xl,
                    xr = max(t),
                    #xr = xr,
                    data = data,
                    spline_nseg_t = spline_nseg_t)
    # Finding derivative  of basis functions using first principals-----------
    first_deriv_calc <- function(t_new) {
      # Create the basis functions
      B_t_deriv <- bs_bbase_t(t_new,
        xl = min(data$Age),
        xr = max(data$Age),
        data = data,
        spline_nseg_t = spline_nseg_t)

      return(B_t_deriv)
    }
    # Now create derivatives----------------------
    h <- 0.001
    first_deriv_step1 <- first_deriv_calc(t + h)
    first_deriv_step2 <- first_deriv_calc(t - h)
    B_t_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)
    #B_t_deriv <- (first_deriv_step1 - first_deriv_step2) / (h)

    # Basis functions in time using prediction data frame-----------------------
    t_pred <- data_grid$Age

    B_t_pred <-
      bs_bbase_t(t_pred,
                 #xl=xl,
                 #xr=xr,
      xl = min(t),
      xr = max(t),
      data = data,
      spline_nseg_t = spline_nseg_t
    )

    # Now create derivatives----------------------
    h <- 0.001
    #h <- 0.00001
    t_pred <- data_grid$Age
    first_deriv_step1 <- first_deriv_calc(t_pred + h)
    first_deriv_step2 <- first_deriv_calc(t_pred - h)
    B_t_pred_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)

    spline_basis_fun_list <- list(
      B_t = B_t,
      B_t_deriv = B_t_deriv,
      B_t_pred = B_t_pred,
      B_t_pred_deriv = B_t_pred_deriv
    )
  }

  if (model_type == "ni_spline_st") {
    t <- data$Age
    # Basis functions in space time for data-----------------------
    B_time <- bs_bbase_st(t,
      xl = min(t),
      xr = max(t),
      data = data,
      spline_nseg_st = spline_nseg_st
    )
    B_space_1 <- bs_bbase_st(data$Latitude,
      xl = min(data$Latitude),
      xr = max(data$Latitude),
      data = data,
      spline_nseg_st = spline_nseg_st
    )
    B_space_2 <- bs_bbase_st(data$Longitude,
      xl = min(data$Longitude),
      xr = max(data$Longitude),
      data = data,
      spline_nseg_st = spline_nseg_st
    )

    B_st_full <- matrix(NA,
      ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
      nrow = nrow(data)
    )
    regional_knots_loc <- rep(NA,
      ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1)
    )
    count <- 1
    for (i in 1:ncol(B_time)) {
      for (j in 1:ncol(B_space_1)) {
        for (k in 1:ncol(B_space_2)) {
          regional_knots_loc[count] <- i
          B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
          count <- count + 1
        }
      }
    }

    # Get rid of all the columns which are just zero
    # B_st <- B_st_full
    B_st <- B_st_full[, -which(colSums(B_st_full) < 0.1)]

    # Find the index here that you remove then use this in the derivative
    remove_col_index <- which(colSums(B_st_full) < 0.1)

    # first_deriv_calc <- function(B_st,t_new) {
    first_deriv_calc <- function(t_new) {
      # B_st_deriv <- predict(object = B_st,newx = t_new)#,x_train = t_old)t_old
      # Now the local basis functions
      B_time <- bs_bbase_st(t_new,
        xl = min(data$Age),
        xr = max(data$Age),
        data = data,
        spline_nseg_st = spline_nseg_st
      )
      B_space_1 <- bs_bbase_st(data$Latitude,
        xl = min(data$Latitude),
        xr = max(data$Latitude),
        data = data,
        spline_nseg_st = spline_nseg_st
      )
      B_space_2 <- bs_bbase_st(data$Longitude,
        xl = min(data$Longitude),
        xr = max(data$Longitude),
        data = data,
        spline_nseg_st = spline_nseg_st
      )

      B_st_full <- matrix(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data)
      )
      regional_knots_loc <- rep(NA, ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1))
      count <- 1
      for (i in 1:ncol(B_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero
      B_st_deriv <- B_st_full[, -which(colSums(B_st_full) < 0.1)]
      return(B_st_deriv)
    }
    # Now create derivatives----
    h <- 0.0001

    first_deriv_step1 <- first_deriv_calc(t_new = t + h)
    first_deriv_step2 <- first_deriv_calc(t_new = t - h)
    B_st_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)


    # Basis functions in space time using prediction data frame-----------------------
    B_pred_time <- bs_bbase_st(data_grid$Age,
      xl = min(data$Age),
      xr = max(data$Age),
      data = data,
      spline_nseg_st = spline_nseg_st
    )
    B_space_1 <- bs_bbase_st(data_grid$Latitude,
      xl = min(data$Latitude),
      xr = max(data$Latitude),
      data = data,
      spline_nseg_st = spline_nseg_st
    )
    B_space_2 <- bs_bbase_st(data_grid$Longitude,
      xl = min(data$Longitude),
      xr = max(data$Longitude),
      data = data,
      spline_nseg_st = spline_nseg_st
    )

    suppressWarnings({
      B_st_pred_full <- matrix(NA,
        ncol = ncol(B_pred_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data_grid)
      )
      regional_knots_loc <- rep(NA,
        ncol = ncol(B_pred_time) * ncol(B_space_1) * ncol(B_space_1)
      )
      count <- 1
      for (i in 1:ncol(B_pred_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_pred_full[, count] <- B_pred_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero corresponding to the previous basis functions
      B_st_pred <- B_st_pred_full[, -remove_col_index]
    })
    # Now create derivatives for prediciton
    first_deriv_calc <- function(t_new) {
      # Now the local basis functions
      B_time <- bs_bbase_st(t_new,
        xl = min(data$Age),
        xr = max(data$Age),
        data = data,
        spline_nseg_st = spline_nseg_st
      )
      B_space_1 <- bs_bbase_st(data_grid$Latitude,
        xl = min(data$Latitude),
        xr = max(data$Latitude),
        data = data,
        spline_nseg_st = spline_nseg_st
      )
      B_space_2 <- bs_bbase_st(data_grid$Longitude,
        xl = min(data$Longitude),
        xr = max(data$Longitude),
        data = data,
        spline_nseg_st = spline_nseg_st
      )

      B_st_full <- matrix(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data_grid)
      )
      regional_knots_loc <- rep(NA, ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1))
      count <- 1
      for (i in 1:ncol(B_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero
      # B_st <- B_st_full[,-which(colSums(B_st_full) < 0.1)]
      B_st <- B_st_full[, -remove_col_index]
      return(B_st)
    }
    h <- 0.0001
    t_pred <- data_grid$Age

    first_deriv_step1 <- first_deriv_calc(t_pred + h)
    first_deriv_step2 <- first_deriv_calc(t_pred - h)
    B_st_deriv_pred <- (first_deriv_step1 - first_deriv_step2) / (2 * h)

    # All Basis Functions
    spline_basis_fun_list <- list(
      remove_col_index = remove_col_index,
      B_st = B_st,
      B_st_deriv = B_st_deriv,
      B_st_pred = B_st_pred,
      B_st_deriv_pred = B_st_deriv_pred
    )
  }

  if (model_type == "ni_gam_decomp") {
    # Basis functions in time for data-----------------------
    B_t <- bs_bbase_t(data$Age,
      xl = min(data$Age),
      xr = max(data$Age),
      spline_nseg_t = spline_nseg_t,
      data = data
    )
    # Finding derivative  of basis functions using first principals-----------
    first_deriv_calc <- function(t_new) {
      # Create the regional basis functions
      B_t <- bs_bbase_t(t_new,
        xl = min(data$Age),
        xr = max(data$Age),
        spline_nseg_t = spline_nseg_t,
        data = data
      )
      return(B_t)
    }
    # Now create derivatives----------------------
    h <- 0.00001 # h <- 0.001
    t <- data$Age
    first_deriv_step1 <- first_deriv_calc(t + h)
    first_deriv_step2 <- first_deriv_calc(t - h)
    B_t_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)

    # Basis functions in time using prediction data frame-----------------------
    B_t_pred <- bs_bbase_t(data_grid$Age,
      xl = min(data$Age),
      xr = max(data$Age),
      spline_nseg_t = spline_nseg_t,
      data = data
    )
    # Now create derivatives----------------------
    h <- 0.00001 # h <- 0.001
    t_pred <- data_grid$Age
    first_deriv_step1 <- first_deriv_calc(t_pred + h)
    first_deriv_step2 <- first_deriv_calc(t_pred - h)
    B_t_pred_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)


    # Basis functions in space time for data-----------------------
    B_time <- bs_bbase_st(data$Age,
      xl = min(data$Age),
      xr = max(data$Age),
      spline_nseg_st = spline_nseg_st,
      data = data
    )
    B_space_1 <- bs_bbase_st(data$Latitude,
      xl = min(data$Latitude),
      xr = max(data$Latitude),
      spline_nseg_st = spline_nseg_st,
      data = data
    )
    B_space_2 <- bs_bbase_st(data$Longitude,
      xl = min(data$Longitude),
      xr = max(data$Longitude),
      spline_nseg_st = spline_nseg_st,
      data = data
    )

    B_st_full <- matrix(NA,
      ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
      nrow = nrow(data)
    )
    regional_knots_loc <- rep(NA,
      ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1)
    )
    count <- 1
    for (i in 1:ncol(B_time)) {
      for (j in 1:ncol(B_space_1)) {
        for (k in 1:ncol(B_space_2)) {
          regional_knots_loc[count] <- i
          B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
          count <- count + 1
        }
      }
    }

    # Get rid of all the columns which are just zero
    B_st <- B_st_full[, -which(colSums(B_st_full) < 0.1)]

    # Find the index here that you remove then use this in the derivative
    remove_col_index <- which(colSums(B_st_full) < 0.1)

    first_deriv_calc <- function(t_new) {
      # Now the local basis functions
      B_time <- bs_bbase_st(t_new,
        xl = min(data$Age),
        xr = max(data$Age),
        spline_nseg_st = spline_nseg_st,
        data = data
        # deg = 3
      )
      B_space_1 <- bs_bbase_st(data$Latitude,
        xl = min(data$Latitude),
        xr = max(data$Latitude),
        spline_nseg_st = spline_nseg_st,
        data = data # ,deg = 3, nseg = 6
        # deg = 3
      )
      B_space_2 <- bs_bbase_st(data$Longitude,
        xl = min(data$Longitude),
        xr = max(data$Longitude),
        spline_nseg_st = spline_nseg_st,
        data = data # ,deg = 3, nseg = 6
        # deg = 3
      )

      B_st_full <- matrix(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data)
      )
      regional_knots_loc <- rep(NA, ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1))
      count <- 1
      for (i in 1:ncol(B_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero
      B_st <- B_st_full[, -which(colSums(B_st_full) < 0.1)]
      return(B_st)
    }
    #-------Now create derivatives----
    h <- 0.00001 # h <- 0.0001
    t <- data$Age

    first_deriv_step1 <- first_deriv_calc(t + h)
    first_deriv_step2 <- first_deriv_calc(t - h)
    B_st_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)


    # Basis functions in space time using prediction data frame-----------------------
    B_pred_time <- bs_bbase_st(data_grid$Age,
      xl = min(data$Age),
      xr = max(data$Age),
      spline_nseg_st = spline_nseg_st,
      data = data # ,deg = 3, nseg = 6
      # deg = 3
    )
    B_space_1 <- bs_bbase_st(data_grid$Latitude,
      xl = min(data$Latitude),
      xr = max(data$Latitude),
      spline_nseg_st = spline_nseg_st,
      data = data # ,deg = 3, nseg = 6
      # deg = 3
    )
    B_space_2 <- bs_bbase_st(data_grid$Longitude,
      xl = min(data$Longitude),
      xr = max(data$Longitude),
      spline_nseg_st = spline_nseg_st,
      data = data # ,deg = 3, nseg = 6
      # deg = 3
    )

    suppressWarnings({
      B_st_pred_full <- matrix(NA,
        ncol = ncol(B_pred_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data_grid)
      ) # Not sure here?? nrow(data)
      regional_knots_loc <- rep(NA,
        ncol = ncol(B_pred_time) * ncol(B_space_1) * ncol(B_space_1)
      )
      count <- 1
      for (i in 1:ncol(B_pred_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_pred_full[, count] <- B_pred_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero corresponding to the previous basis functions
      B_st_pred <- B_st_pred_full[, -remove_col_index]
    })
    #-------Now create derivatives for prediciton----
    first_deriv_calc <- function(t_new) {
      # Now the local basis functions
      B_time <- bs_bbase_st(t_new,
        xl = min(data$Age),
        xr = max(data$Age),
        spline_nseg_st = spline_nseg_st,
        data = data
        # deg = 3
      )
      B_space_1 <- bs_bbase_st(data_grid$Latitude,
        xl = min(data$Latitude),
        xr = max(data$Latitude),
        spline_nseg_st = spline_nseg_st,
        data = data # ,deg = 3, nseg = 6
        # deg = 3
      )
      B_space_2 <- bs_bbase_st(data_grid$Longitude,
        xl = min(data$Longitude),
        xr = max(data$Longitude),
        spline_nseg_st = spline_nseg_st,
        data = data # ,deg = 3, nseg = 6
        # deg = 3
      )

      B_st_full <- matrix(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data_grid)
      ) # nrow(data))
      regional_knots_loc <- rep(NA, ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1))
      count <- 1
      for (i in 1:ncol(B_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero
      # B_st <- B_st_full[,-which(colSums(B_st_full) < 0.1)]
      B_st <- B_st_full[, -remove_col_index] # Not sure here
      return(B_st)
    }
    h <- 0.00001 # h <- 0.001
    t_pred <- data_grid$Age

    first_deriv_step1 <- first_deriv_calc(t_pred + h)
    first_deriv_step2 <- first_deriv_calc(t_pred - h)
    B_st_deriv_pred <- (first_deriv_step1 - first_deriv_step2) / (2 * h)

    # Derivative of Basis function for the total model fit-----------------
    first_deriv_calc <- function(t_new) {
      # Create the regional basis functions
      B_t <- bs_bbase_t(t_new,
        xl = min(data$Age),
        xr = max(data$Age),
        spline_nseg_t = spline_nseg_t,
        data = data
      ) # nseg = 10)
      colnames(B_t) <- c(paste("B_t", 1:ncol(B_t), sep = ""))

      # Now the local basis functions
      B_time <- bs_bbase_st(t_new,
        xl = min(data$Age),
        xr = max(data$Age),
        spline_nseg_st = spline_nseg_st,
        data = data # , deg = 3,nseg = 6
        # deg = 3
      )
      B_space_1 <- bs_bbase_st(data$Latitude,
        xl = min(data$Latitude),
        xr = max(data$Latitude),
        spline_nseg_st = spline_nseg_st,
        data = data # , deg = 3,nseg = 6
        # deg = 3
      )
      B_space_2 <- bs_bbase_st(data$Longitude,
        xl = min(data$Longitude),
        xr = max(data$Longitude),
        spline_nseg_st = spline_nseg_st,
        data = data # ,deg = 3, nseg = 6
        # deg = 3
      )

      B_st_full <- matrix(NA,
        ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1),
        nrow = nrow(data)
      )
      regional_knots_loc <- rep(NA, ncol = ncol(B_time) * ncol(B_space_1) * ncol(B_space_1))
      count <- 1
      for (i in 1:ncol(B_time)) {
        for (j in 1:ncol(B_space_1)) {
          for (k in 1:ncol(B_space_2)) {
            regional_knots_loc[count] <- i
            B_st_full[, count] <- B_time[, i] * B_space_1[, j] * B_space_2[, k]
            count <- count + 1
          }
        }
      }

      # Get rid of all the columns which are just zero
      B_st <- B_st_full[, -which(colSums(B_st_full) < 0.1)]
      colnames(B_st) <- c(paste("B_st", 1:ncol(B_st), sep = ""))
      # Dummy matrix for intercept & GIA
      B_h <- fastDummies::dummy_cols(data.frame(data$SiteName))
      B_h <- B_h[, -1]
      colnames(B_h) <- c(paste("B_h", 1:ncol(B_h), sep = ""))
      B_g <- B_h * t_new
      colnames(B_g) <- c(paste("B_g", 1:ncol(B_g), sep = ""))
      # Basis function matrix with B_local & B_regional
      output_B_tot <- cbind(
        B_h, B_g,
        B_t, B_st
      )


      return(output_B_tot)
    }
    #-------Now create derivatives----
    h <- 0.00001 # h <- 0.0001
    t <- data$Age

    first_deriv_step1 <- first_deriv_calc(t + h)
    first_deriv_step2 <- first_deriv_calc(t - h)
    B_tot_deriv <- (first_deriv_step1 - first_deriv_step2) / (2 * h)

    # New basis function for site specific vertical offset----------
    B_h_deriv <- as.matrix(B_tot_deriv[, grepl("B_h", names(B_tot_deriv))])
    # New Basis Functions for Random linear local component-Site Specific slope-------
    B_deriv_g_re <- as.matrix(B_tot_deriv[, grepl("B_g", names(B_tot_deriv))])
    # New Basis Functions for spline in time-------------------------
    B_deriv_t <- as.matrix(B_tot_deriv[, grepl("B_t", names(B_tot_deriv))])
    # New Basis Functions in Space time-------------------------------
    B_deriv_st <- as.matrix(B_tot_deriv[, grepl("B_st", names(B_tot_deriv))])

    # All Basis Functions
    spline_basis_fun_list <- list(
      remove_col_index = remove_col_index,
      B_st = B_st,
      B_st_deriv = B_st_deriv,
      B_st_pred = B_st_pred,
      B_st_deriv_pred = B_st_deriv_pred,
      B_t = B_t,
      B_t_deriv = B_t_deriv,
      B_t_pred = B_t_pred,
      B_t_pred_deriv = B_t_pred_deriv,
      B_h_deriv = B_h_deriv,
      B_deriv_g_re = B_deriv_g_re,
      B_tot_deriv = B_tot_deriv,
      B_deriv_t = B_deriv_t,
      B_deriv_st = B_deriv_st
    )
  }



  return(spline_basis_fun_list)
}

#' Creating spline basis functions for spline in time
#'
#' @param x Age in years CE
#' @param xl minimum Age
#' @param xr maximum Age
#' @param spline_nseg_t number of sections
#' @param deg Degree of polynomial
#' @param data Input data
#' @noRd
# Basis function approach
bs_bbase_t <- function(x,
                     xl = min(x),
                     xr = max(x),
                     deg = 3,
                     spline_nseg_t = NULL,
                     data = data) {
  # Create basis functions------------------------------------------------------
   if (is.null(spline_nseg_t)) {
     spline_nseg_t <- round(deg / (1 + deg / length(data$Age)))
    }

  # Compute the length of the partitions
  dx <- (xr - xl) / spline_nseg_t
  # Create equally spaced knots
  knots <- seq(xl - deg * dx,
    xr + deg * dx,
    by = dx
  )
  print(length(knots))

  # Use bs() function to generate the B-spline basis
  get_bs_matrix <- matrix(
    splines::bs(x,
      degree = deg,
      knots = knots,
      Boundary.knots = c(knots[1], knots[length(knots)])
    ),
    nrow = length(x)
  )
  # Remove columns that contain zero only
  #bs_matrix <- get_bs_matrix[, -c(1:deg, ncol(get_bs_matrix):(ncol(get_bs_matrix) - deg))]
  bs_matrix <-get_bs_matrix
  return(bs_matrix)
}

#' Creating spline basis functions for spline in space time
#'
#' @param x Age in years CE
#' @param xl minimum Age
#' @param xr maximum Age
#' @param spline_nseg_st number of sections
#' @param deg Degree of polynomial
#' @param data Input data
#' @noRd
# Basis function approach
bs_bbase_st <- function(x,
                       xl=min(x),#xl
                       xr=max(x),#xr
                       deg = 3,
                       spline_nseg_st = NULL,
                       data = data) {
  # Create basis functions------------------------------------------------------
  if (is.null(spline_nseg_st)) {
    spline_nseg_st <- round(deg / (1 + deg / length(data$Age)))
  }

  # Compute the length of the partitions
  dx <- (xr - xl) / spline_nseg_st
  # Create equally spaced knots
  knots <- seq(xl - deg * dx,
               xr + deg * dx,
               by = dx
  )
  print(length(knots))

  # Use bs() function to generate the B-spline basis
  get_bs_matrix <- matrix(
    splines::bs(x,
                degree = deg,
                knots = knots,
                Boundary.knots = c(knots[1], knots[length(knots)])
    ),
    nrow = length(x)
  )
  # Remove columns that contain zero only
  #bs_matrix <- get_bs_matrix[, -c(1:deg, ncol(get_bs_matrix):(ncol(get_bs_matrix) - deg))]
  bs_matrix <-get_bs_matrix
  return(bs_matrix)
}

# Think about using this one?
# These functions create the B-spline basis functions
# They are taken from the Eilers and Marx 'Craft of smoothing' course
# http://statweb.lsu.edu/faculty/marx/
# tpower <- function(x, t, p) {
#   # Truncated p-th power function
#   return((x - t)^p * (x > t))
# }
# bs_bbase <- function(x, xl = min(x), xr = max(x),
#                      nseg = NULL,#30,
#                      deg = 3,data) {
#     if (is.null(nseg)) {
#      nseg <- round(deg / (1 + deg / length(data$Age)))
#     }
#   # Construct B-spline basis
#   dx <- (xr - xl) / nseg
#   knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
#   P <- outer(x, knots, tpower, deg)
#   n <- dim(P)[2]
#   D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx^deg)
#   B <- (-1)^(deg + 1) * P %*% t(D)
#   return(B)
# }
