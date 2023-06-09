% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reslr_load.R
\name{reslr_load}
\alias{reslr_load}
\title{Loading in data for the \code{reslr} package}
\usage{
reslr_load(
  data,
  prediction_grid_res = 50,
  include_tide_gauge = FALSE,
  include_linear_rate = FALSE,
  list_preferred_TGs = NULL,
  TG_minimum_dist_proxy = FALSE,
  all_TG_1deg = FALSE,
  input_age_type = "CE",
  sediment_average_TG = 10,
  detrend_data = FALSE,
  core_col_year = NULL,
  cross_val = FALSE,
  test_set
)
}
\arguments{
\item{data}{The input data as a dataframe.}

\item{prediction_grid_res}{Resolution of grid. Predictions over every 50 years(default) can vary based on user preference, as larger values will reduce computational run time.}

\item{include_tide_gauge}{Including tide gauge data from PSMSL website. The tide gauge data undergo a cleaning process in this function where flagged stations are removed as recommended by the online database. Next, the tide gauge data is averaged over period defined by sediment_average_TG which default is 10 years corresponding to accumulation rates of proxy records. Then, the user selects their preferred tide gauge based on three criteria: 1.nearest tide gauge to the proxy site; 2. User supplies a list of names of preferred tide gauges; 3. all tide gauges within 1 degree are chosen.}

\item{include_linear_rate}{User decides to include linear_rate and linear_rate_err associated. This relates to linear rate which corresponds to an important physical process that impacts sea level observations which is glacial isostatic adjustment (GIA). For the linear_rate and its associated linear_rate_err, the user can provide these values as additional columns in the input dataframe. If they prefer, the package will calculate the linear_rate and the linear_rate_err using the data.}

\item{list_preferred_TGs}{The user can supply the name or names of the preferred tide gauges from the PSMSL database.}

\item{TG_minimum_dist_proxy}{The package finds the tide gauge closest to the proxy site}

\item{all_TG_1deg}{The package finds all tide gauges within 1 degree of the proxy site}

\item{input_age_type}{The inputted age in years "CE" or year "BP". The default is "CE" and is the preferred structure of the package. The package has the ability to use Before Present ("BP") observations.}

\item{sediment_average_TG}{Average the tide gauge data to make it comparable to accumulation rates of proxy records. The default averaging period for tide gauges is 10 years and the user can alter this.}

\item{detrend_data}{Detrend the data using the linear rate provided to remove this component.}

\item{core_col_year}{The year the sediment core was collected in order to the data to be detrended.}

\item{cross_val}{For the spline in time, spline in space time and the NIGAM the user can undertake cross validation to examine the}

\item{test_set}{The test set dataframe for cross validation}
}
\value{
A list containing data frame of data and prediction grid. The output of this function is two data frames, one with the data and one with the data_grid which represent a grid with evenly spaced time points. If tide gauge data is used, an ID column is included in the two output dataframes displaying the data source, "ProxyRecord" or "TideGaugeData".
}
\description{
In this function, the data provided by the user is loaded into the package.
The prerequisites of the input data structure has been given in the vignettes.
The user can choose to include data from tide gauges which is sourced from the Permanent Service for Mean Sea Level (PSMSL) online database.
}
\examples{
\donttest{
data <- NAACproxydata \%>\% dplyr::filter(Site == "Cedar Island")
reslr_load(data = data)}
}
