## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "vig/"
)
options(rmarkdown.html_vignette.check_title = FALSE)
# library(tidyverse)

## ----eval = FALSE,message=FALSE-----------------------------------------------
#  # install.packages("reslr")
#  # library(devtools)
#  # devtools::install()
#  devtools::install_github("maeveupton/reslr")

## ---- readpkg,eval = TRUE, message=FALSE--------------------------------------
library(reslr)

## ---- example1site,eval = TRUE------------------------------------------------
# For 1 site
data_1site <- reslr::NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
# For multiple sites
data_multisite <- reslr::NAACproxydata %>% dplyr::filter(Site %in% c(
  "Snipe Key", "Cheesequake",
  "Placentia", "Leeds Point"
))

## ---- example1dataagain, eval = TRUE------------------------------------------
# For 1 site
CedarIslandNC <- reslr::NAACproxydata %>% dplyr::filter(Site == "Cedar Island")

## ---- loadslr, eval = TRUE----------------------------------------------------
CedarIslandNC_input <- reslr_load(
  data = CedarIslandNC,
  include_tide_gauge = FALSE,
  include_linear_rate = FALSE,
  TG_minimum_dist_proxy = FALSE,
  list_preferred_TGs = NULL,
  all_TG_1deg = FALSE,
  prediction_grid_res = 50,
  input_Age_type = "CE",
  sediment_average_TG = 10
)

## ---- slrdata,eval=TRUE-------------------------------------------------------
data <- CedarIslandNC_input$data

## ---- datagridslr, eval = TRUE------------------------------------------------
data_grid <- CedarIslandNC_input$data_grid

## ---- printdata, eval=TRUE----------------------------------------------------
print(CedarIslandNC_input)

## ---- plotdata,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(
  x = CedarIslandNC_input,
  title = "Plot of the raw data",
  xlab = "Year (CE)",
  ylab = "Relative Sea Level (m)",
  plot_tide_gauges = FALSE,
  plot_proxy_records = TRUE,
  plot_caption = TRUE
)

## ---- runslr,eval = TRUE------------------------------------------------------
res_eiv_slr_t <- reslr_mcmc(
  input_data = CedarIslandNC_input,
  model_type = "eiv_slr_t",
  CI = 0.95
)

## ---- printrunslr,eval=TRUE---------------------------------------------------
print(res_eiv_slr_t)

## ---- summaryslr, eval = TRUE-------------------------------------------------
summary(res_eiv_slr_t)

## ---- runslrmoreit,eval = FALSE-----------------------------------------------
#  res_eiv_slr_t <- reslr_mcmc(
#    input_data = CedarIslandNC_input,
#    model_type = "eiv_slr_t",
#    # Update these values
#    n_iterations = 6000, # Number of iterations
#    n_burnin = 1000, # Number of iterations to discard at the beginning
#    n_thin = 4, # Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#  )

## ---- plotslrres, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res_eiv_slr_t,
  xlab = "Year (CE)",
  ylab = "Relative Sea Level (m)"
)

## ---- dataframeslrres, eval = TRUE--------------------------------------------
output_dataframes <- res_eiv_slr_t$output_dataframes
head(output_dataframes)

## ----exampledata1, eval = TRUE------------------------------------------------
# For 1 site
CedarIslandNC <- reslr::NAACproxydata %>% dplyr::filter(Site == "Cedar Island")

## ----loaddatacp, eval = TRUE--------------------------------------------------
CedarIslandNC_input <- reslr_load(
  data = CedarIslandNC,
  include_tide_gauge = FALSE,
  include_linear_rate = FALSE,
  TG_minimum_dist_proxy = FALSE,
  list_preferred_TGs = NULL,
  all_TG_1deg = FALSE,
  prediction_grid_res = 50,
  sediment_average_TG = 10
)

## ----datacp,eval=TRUE---------------------------------------------------------
data <- CedarIslandNC_input$data
head(data)

## ----datagridcp, eval = TRUE--------------------------------------------------
data_grid <- CedarIslandNC_input$data_grid
head(data_grid)

## ----printcp, eval=TRUE-------------------------------------------------------
print(CedarIslandNC_input)

## ----plotdatacp,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(
  x = CedarIslandNC_input,
  title = "Plot of the raw data",
  xlab = "Year (CE)",
  ylab = "Relative Sea Level (m)",
  plot_proxy_records = TRUE,
  plot_tide_gauges = FALSE
)

## ----runcp1, eval = TRUE------------------------------------------------------
res_eiv_cp1_t <- reslr_mcmc(
  input_data = CedarIslandNC_input,
  model_type = "eiv_cp_t",
  n_cp = 1,
  CI = 0.95
)

## ----runcp2, eval = FALSE-----------------------------------------------------
#  res_eiv_cp2_t <- reslr_mcmc(
#    input_data = CedarIslandNC_input,
#    model_type = "eiv_cp_t",
#    n_cp = 2, # Updating the default setting to include an additional change point.
#    CI = 0.95
#  )

## ----printcpout, eval=TRUE----------------------------------------------------
print(res_eiv_cp1_t)

## ----summarycp, eval = TRUE---------------------------------------------------
summary(res_eiv_cp1_t)

## ----runcpmoreit,eval = FALSE-------------------------------------------------
#  res_eiv_cp1_t <- reslr_mcmc(
#    input_data = CedarIslandNC_input,
#    model_type = "eiv_cp_t",
#    # Update these values
#    n_iterations = 6000, # Number of iterations
#    n_burnin = 1000, # Number of iterations to discard at the beginning
#    n_thin = 4, # Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#  )

## ---- plotcpres, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res_eiv_cp1_t,
  xlab = "Year (CE)",
  ylab = "Relative Sea Level (m)",
)

## ----cpdataframe, eval = TRUE-------------------------------------------------
output_dataframes <- res_eiv_cp1_t$output_dataframes
head(output_dataframes)

## ----exampledataset2 ,eval = TRUE---------------------------------------------
# For 1 site
CedarIslandNC <- reslr::NAACproxydata %>% dplyr::filter(Site == "Cedar Island")

## ---- loadigp ,eval = TRUE----------------------------------------------------
CedarIslandNC_input <- reslr_load(
  data = CedarIslandNC,
  include_tide_gauge = FALSE,
  include_linear_rate = FALSE,
  TG_minimum_dist_proxy = FALSE,
  list_preferred_TGs = NULL,
  all_TG_1deg = FALSE,
  prediction_grid_res = 50,
  sediment_average_TG = 10
)

## ----dataigp,eval=TRUE--------------------------------------------------------
data <- CedarIslandNC_input$data

## ----datagridigp, eval = TRUE-------------------------------------------------
data_grid <- CedarIslandNC_input$data_grid

## ----printigp, eval=TRUE------------------------------------------------------
print(CedarIslandNC_input)

## ---- plotigpdata,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(
  x = CedarIslandNC_input,
  title = "Plot of the raw data",
  xlab = "Year (CE)",
  ylab = "Relative Sea Level (m)",
  plot_proxy_records = TRUE,
  plot_tide_gauges = FALSE
)

