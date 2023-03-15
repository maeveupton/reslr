#' Adding linear rate to the sites from Tide Gauges
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
      linear_rate_err = summary(stats::lm(RSL ~ Age))$coefficients[2, 2]
    )


  # Table of GIA rate vs lm rate from proxy data
  lm_slopes <- data_lm %>%
    dplyr::select(SiteName, linear_rate, linear_rate_err) %>%
    unique()
  return(lm_slopes)
}
