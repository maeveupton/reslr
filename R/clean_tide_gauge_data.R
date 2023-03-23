#' Including tide gauge data
#'
#' @param data Input data
#The user can supply the name or names of the preferred tide gauges
#' @noRd
clean_tidal_gauge_data <- function(data
                                   #name_chosesn_TGs = "FALSE"
                                   ) {
  Age_epoch_id <-  LongLat <-  nearest_proxy_site<- RSL_annual<- minimum_dist<-nearest_TG<-rows_site<-site<-min_dist1<-stationflag<-name<-sd<-sd_TG<- n_obs_by_site<-RSL_offset <- data_type_id <-decade<- decade_meanRSL<- Age <- RSL <- Age_err<- RSL_err <- linear_rate <- linear_rate_err <-SiteName <- Longitude <- Latitude <- id <- NULL
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

  ###------------Loop to open all RSL & Age data files------------
  read_plus <- function(flnm) {
    data.table::fread(flnm, sep= ";") %>% # fread quicker way to read in & allows for ; to be used
      dplyr::mutate(filename = flnm) # allows you to include the file name as id
  }
  # Warnings: there are some files without data
  suppressWarnings(
    temp_SL<-
      list.files(path = file.path(temp_dir, "rlr_annual", "data"),
                 #path = "rlr_annual/data",
                 pattern = "*.rlrdata",
                 full.names = T) %>%
      purrr::map_df(~read_plus(.)) %>%
      dplyr::tibble())

  colnames(temp_SL) = c("Age","RSL","flag_attention_1","flag_attention_2","id")
  temp_SL$id <- stringr::str_extract(basename(temp_SL$id),"[0-9]+")

  # Access the individual data files within the 'rlr_annual' folder
  file_path <- file.path(temp_dir, "rlr_annual", "filelist.txt")
  file_list <- utils::read.csv(file_path,stringsAsFactors = FALSE, header=F,sep=";")
  colnames(file_list)<- c("id","Latitude","Longitude","name","coastline","stationcode","stationflag")
  # Removing white space in the name of each site
  file_list$name <- stringr::str_trim(file_list$name,side = "both")
  file_list$stationflag <-  stringr::str_trim(file_list$stationflag,side = "both")
  # Data from the PSMSL website
  data_TG <- temp_SL %>%
    # pulling out the file number from string so that it matches the name from other files
    dplyr::mutate(id = stringr::str_extract(basename(temp_SL$id),"[0-9]+")) %>%
    # Cases where bad data was collected
    dplyr::filter(!RSL== -99999) %>%
    dplyr::group_by(id) %>%
    #2000-2018 used as the tidal epoch
    dplyr::mutate(Age_epoch_id = ifelse(dplyr::between(Age,2000,2018),TRUE,FALSE))

  # Removing offset based on the location---
  # Offset value is the mean of RSL over the tidal epoch
  # Setting 2000-2018 as the tidal epoch
  Age_epoch_ref <-  data_TG %>%
    dplyr::select(RSL,Age_epoch_id) %>%
    dplyr::filter(Age_epoch_id == TRUE) %>%
    dplyr::summarise(RSL_offset  = unique(mean(RSL)))

  data_TG <- merge(data_TG,Age_epoch_ref,by = "id",all=TRUE)
  # Cases where no data between 2000-2018 set the offset to 7000
  data_TG$RSL_offset[is.na(data_TG$RSL_offset)] <- 7000

  # Updating the RSL to the shifted RSL value
  data_TG$RSL <- data_TG$RSL - data_TG$RSL_offset

  #--Joining SL data with location names--
  annual_SL_tide_df <-merge(data_TG,file_list,by = "id",all = TRUE)
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
    dplyr::arrange(SiteName, .by_group = TRUE)


  # # Set the window size for the moving average (in this case, 10 years)
  # window_size <- 10
  #
  # # Create a new column with the rolling average
  # annual_tidal_gauge_data_df$rolling_avg <- zoo::rollapply(annual_tidal_gauge_data_df$RSL,
  #                                                          width = window_size,
  #                                                          FUN = mean,
  #                                                          align = "right",
  #                                                          fill = NA)
  #
  # # create a new column for the decade based on the midpoint of the rolling window
  # annual_tidal_gauge_data_df$decade <- as.integer(floor((annual_tidal_gauge_data_df$Age - (window_size/2))/10)*10)
  #
  # # calculate the decadal averages based on the rolling average
  # decadal_averages_TG <-
  #   annual_tidal_gauge_data_df %>%
  #   dplyr::group_by(decade, SiteName) %>%
  #   dplyr::summarise(
  #     decade_meanRSL = mean(rolling_avg, na.rm = TRUE),
  #     Age = max(Age),
  #     rows_site = dplyr::n()
  #   )



  #----Decadal Averages------
  decadal_averages_TG <-
    annual_tidal_gauge_data_df %>%
    dplyr::mutate(decade = (Age - 1) %/% 10) %>%
    dplyr::group_by(decade, SiteName) %>%
    dplyr::summarise(
      decade_meanRSL = mean(RSL),
      Age = max(Age),
      rows_site = dplyr::n()
    ) # Age=min(Age)

  #---Using standard deviation of RSL as uncertainty----
  decadal_averages_TG <- decadal_averages_TG %>%
    dplyr::group_by(SiteName) %>%
    dplyr::mutate(sd_TG = sd(decade_meanRSL))

  #----- New df with decadal averages for tide gauges-----
  tidal_gauge_average_10_df <- merge(decadal_averages_TG, annual_tidal_gauge_data_df)

  #---Rsl & Age error for tidal gauge data----
  tidal_gauge_full_df <- tidal_gauge_average_10_df %>%
    dplyr::mutate(
      Age_err= 5, # years --> half a year/half a decade
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
    dplyr::mutate(RSL = decade_meanRSL)

  # Criteria 1: Removing sites with only 2 points (20 years of data)-----
  decadal_NA_TG_df <-
    tidal_gauge_full_df %>%
    #decadal_NA_TG %>%
    dplyr::group_by(SiteName) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::select(!decade, decade_meanRSL, RSL_annual)
  # Criteria 2: User provides a list of TGs------------------------


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

  SL_tide_site_df <- decadal_NA_TG_df %>%
    # dplyr::select(!all_tidal_data_sites) %>%
    dplyr::mutate(Longitude = round(Longitude, 1)) %>%
    dplyr::mutate(Latitude = round(Latitude, 1)) %>%
    tidyr::unite("LongLat", Latitude:Longitude, remove = FALSE) %>% # Uniting 2 columns
    dplyr::mutate(site = sprintf("%02d", as.integer(as.factor(LongLat)))) %>%
    dplyr::mutate(data_type_id = "TideGaugeData")%>%
    dplyr::group_by(SiteName) %>%
    dplyr::mutate(n_obs_by_site = dplyr::n()) %>%
    dplyr::ungroup()

#
#   #------Joining proxy dataframe to Tide gauges data----
#   SL_tide_proxy <- dplyr::bind_rows(SL_site_df, SL_tide_site_df)

  #------Joining proxy sites to gauges based on shortest distance----
  SL_proxy_unique <- SL_site_df %>%
    dplyr::select(SiteName, Longitude, Latitude, data_type_id,n_obs_by_site) %>%
    unique() %>% as.data.frame()
  SL_tide_unique <- SL_tide_site_df %>%
    dplyr::select(SiteName, Longitude, Latitude, data_type_id,n_obs_by_site) %>%
    unique() %>% as.data.frame()

  #---Distance Matrix for each site to each other---
  mat.distance<- geosphere::distm(SL_proxy_unique[,2:3],SL_tide_unique[,2:3])
  #fun = distGeo)
  #mat.distance <-  as.data.frame(mat.distance)
  mat.distance_m <- as.matrix(mat.distance)
  #--finding row mins & corresponding tidal gauge--
  rownames(mat.distance_m) = SL_proxy_unique$SiteName
  colnames(mat.distance_m) = SL_tide_unique$SiteName

  #--finding row mins & corresponding tidal gauge--
  dist_TG_proxy <- t(sapply(seq(nrow(mat.distance_m)), function(z) {
    js <- order((mat.distance_m[z,]))[1:4]
    c(rownames(mat.distance_m)[z], colnames(mat.distance_m)[js[1]], mat.distance_m[z,js[1]],
      colnames(mat.distance_m)[js[2]], mat.distance_m[z,js[2]],
      colnames(mat.distance_m)[js[3]], mat.distance_m[z,js[3]],
      colnames(mat.distance_m)[js[4]], mat.distance_m[z,js[4]])
  }))

  dist_TG_proxy <- as.data.frame(dist_TG_proxy)
  colnames(dist_TG_proxy) <- c("nearest_proxy_site", "SiteName1", "min_dist1",
                               "SiteName_duplicate2", "min_dist2","SiteName_duplicate3",
                               "min_dist3","SiteName_duplicate4","min_dist4")
  # Sorting the minimum distances from lowest to highest
  dist_TG_proxy <- dist_TG_proxy %>% dplyr::arrange(dplyr::desc(min_dist1))

  dist_TG_proxy_long_1 <- dist_TG_proxy %>%
    tidyr::pivot_longer(cols = dplyr::starts_with(c("min_dist")),
                        values_to = c("minimum_distance"))
  dist_TG_proxy_long_2 <- dist_TG_proxy %>%
    tidyr::pivot_longer(cols = dplyr::starts_with(c("SiteName")),
                        values_to = c("nearest_TG"))
  obs_sites <- SL_tide_unique %>%
    dplyr::filter(SiteName %in% dist_TG_proxy_long_2$nearest_TG) %>%
    dplyr::select(n_obs_by_site)
  # Criteria
  dist_TG_proxy_df_new <- data.frame(nearest_proxy_site = dist_TG_proxy_long_1$nearest_proxy_site,
                                     nearest_TG = dist_TG_proxy_long_2$nearest_TG,
                                     minimum_dist = as.numeric(dist_TG_proxy_long_1$minimum_distance),
                                     n_obs_tg = obs_sites)


  # # TG near the proxy sites & TG longer than 150 years (New York(The Battery))
  # all_nearest_TG <- dist_TG_proxy %>%
  #   dplyr::select(!c(nearest_proxy_site)) %>%
  #   tidyr::pivot_longer(
  #     cols = starts_with("SiteName"),
  #     values_to = "SiteName"
  #   ) %>%
  #   dplyr::select(!name) %>%
  #   tidyr::pivot_longer(
  #     cols = starts_with("min_dist"),
  #     values_to = "MinimumDistance"
  #   )

  # 1 degree away from proxy is 111.1km
  #all_nearest_TG_closest <- all_nearest_TG %>% dplyr::filter(MinimumDistance > 111100)

  # Finding the closest TG
  all_nearest_TG_closest <- dist_TG_proxy_df_new %>%
    dplyr::group_by(nearest_proxy_site) %>%
    dplyr::filter(minimum_dist == min(minimum_dist)) %>%
    dplyr::distinct(nearest_TG,.keep_all = TRUE)# Removing any duplicate tide gauge sites.

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
      RSL_annual, Age_epoch_id,
      RSL_offset, sd_TG, rows_site,
      decade_meanRSL,#rolling_avg,
      n_obs_by_site,site
      # Indicator,Basin,
    )) %>%
    dplyr::mutate(SiteName = as.factor(SiteName))
  additional_datasets <-
    list(annual_tidal_gauge_data_df = annual_tidal_gauge_data_df,
       data=data)
  #return(data)
  return(additional_datasets)
}
