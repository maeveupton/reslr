## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "vig/"
)
options(rmarkdown.html_vignette.check_title = FALSE)
#library(tidyverse)

## ----eval = FALSE,message=FALSE-----------------------------------------------
#  #install.packages("reslr")
#  #library(devtools)
#  #devtools::install()
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
example_data_set <- reslr::NAACproxydata
# For 1 site
CedarIslandNC <- example_data_set %>% dplyr::filter(Site == "Cedar Island")

## ---- loadslr, eval = TRUE----------------------------------------------------
CedarIslandNC_input <- reslr_load(data = CedarIslandNC,
                                  include_tide_gauge = FALSE,
                                  include_linear_rate = FALSE,
                                  TG_minimum_dist_proxy = FALSE,
                                  list_preferred_TGs = NULL,
                                  all_TG_1deg = FALSE,
                                  prediction_interval = 50,
                                  input_Age_type = "CE",
                                  rolling_window_average = 10)

## ---- slrdata,eval=TRUE-------------------------------------------------------
data <- CedarIslandNC_input$data

## ---- datagridslr, eval = TRUE------------------------------------------------
data_grid <- CedarIslandNC_input$data_grid

## ---- printdata, eval=TRUE----------------------------------------------------
print(CedarIslandNC_input)

## ---- plotdata,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(CedarIslandNC_input)

## ----plotdataextended,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(
  x = CedarIslandNC_input,
  title = "Plot of the raw data",
  xlab = "Age (CE)",
  ylab = "Relative Sea Level (m)",
  plot_tide_gauges = FALSE
)

## ---- runslr,eval = TRUE------------------------------------------------------
res.eiv_slr_t <- reslr_mcmc(
  input_data =  CedarIslandNC_input,
  model_type = "eiv_slr_t",
  CI = "95%"
)

## ---- printrunslr,eval=TRUE---------------------------------------------------
print(res.eiv_slr_t)

## ---- summaryslr, eval = TRUE-------------------------------------------------
summary(res.eiv_slr_t)

## ---- runslrmoreit,eval = FALSE-----------------------------------------------
#  res.eiv_slr_t <- reslr_mcmc(
#    input_data =  CedarIslandNC_input,
#    model_type = "eiv_slr_t",
#    # Update these values
#    n_iterations = 6000,# Number of iterations
#    n_burnin = 1000,# Number of iterations to discard at the beginning
#    n_thin = 4,# Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#    )

## ---- plotslrres, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.eiv_slr_t)

## ---- dataframeslrres, eval = TRUE--------------------------------------------
output_dataframes <- res.eiv_slr_t$output_dataframes
head(output_dataframes)

## ----exampledata1, eval = TRUE------------------------------------------------
# For 1 site
CedarIslandNC <- example_data_set %>% dplyr::filter(Site == "Cedar Island")

## ----loaddatacp, eval = TRUE--------------------------------------------------
CedarIslandNC_input <- reslr_load(data = CedarIslandNC,
                                  include_tide_gauge = FALSE,
                                  include_linear_rate = FALSE,
                                  TG_minimum_dist_proxy = FALSE,
                                  list_preferred_TGs = NULL,
                                  all_TG_1deg = FALSE,
                                  prediction_interval = 50,
                                  rolling_window_average = 10)

## ----datacp,eval=TRUE---------------------------------------------------------
data <- CedarIslandNC_input$data
head(data)

## ----datagridcp, eval = TRUE--------------------------------------------------
data_grid <- CedarIslandNC_input$data_grid
head(data_grid)

## ----printcp, eval=TRUE-------------------------------------------------------
print(CedarIslandNC_input)

## ----plotdatacp,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(CedarIslandNC_input)

## ----plotdatacpmore,fig.align = 'center',fig.width = 7,fig.height = 5,eval = FALSE----
#  plot(
#    x =  CedarIslandNC_input,
#    title = "Plot of the raw data",
#    xlab = "Age (CE)",
#    ylab = "Relative Sea Level (m)",
#    plot_tide_gauges = FALSE
#  )

## ----runcp1, eval = TRUE------------------------------------------------------
res.eiv_cp1_t <- reslr_mcmc(
  input_data  = CedarIslandNC_input,
  model_type = "eiv_cp_t", 
  n_cp = 1,
  CI = "95%"
)

## ----runcp2, eval = FALSE-----------------------------------------------------
#  res.eiv_cp2_t <- reslr_mcmc(
#    input_data =  CedarIslandNC_input,
#    model_type = "eiv_cp_t",
#    n_cp = 2, # Updating the default setting to include an additional change point.
#    CI = "95%"
#  )

## ----printcpout, eval=TRUE----------------------------------------------------
print(res.eiv_cp1_t)

## ----summarycp, eval = TRUE---------------------------------------------------
summary(res.eiv_cp1_t)

## ----runcpmoreit,eval = FALSE-------------------------------------------------
#  res.eiv_cp1_t <- reslr_mcmc(
#    input_data =  CedarIslandNC_input,
#    model_type = "eiv_cp_t",
#    # Update these values
#    n_iterations = 6000,# Number of iterations
#    n_burnin = 1000,# Number of iterations to discard at the beginning
#    n_thin = 4,# Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#    )

## ---- plotcpres, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.eiv_cp1_t)

## ----cpdataframe, eval = TRUE-------------------------------------------------
output_dataframes <- res.eiv_cp1_t$output_dataframes
head(output_dataframes)

## ----exampledataset2 ,eval = TRUE---------------------------------------------
# For 1 site
CedarIslandNC <- example_data_set %>% dplyr::filter(Site == "Cedar Island")

## ---- loadigp ,eval = TRUE----------------------------------------------------
CedarIslandNC_input <- reslr_load(data = CedarIslandNC,
                                  include_tide_gauge = FALSE,
                                  include_linear_rate = FALSE,
                                  TG_minimum_dist_proxy = FALSE,
                                  list_preferred_TGs = NULL,
                                  all_TG_1deg = FALSE,
                                  prediction_interval = 50,
                                  rolling_window_average = 10)

## ----dataigp,eval=TRUE--------------------------------------------------------
data <- CedarIslandNC_input$data

## ----datagridigp, eval = TRUE-------------------------------------------------
data_grid <- CedarIslandNC_input$data_grid

## ----printigp, eval=TRUE------------------------------------------------------
print(CedarIslandNC_input)

## ---- plotigpdata,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(CedarIslandNC_input)

## ---- plotigpdatamore,fig.align = 'center',fig.width = 7,fig.height = 5,eval = FALSE----
#  plot(
#    x =  CedarIslandNC_input,
#    title = "Plot of the raw data",
#    xlab = "Age (CE)",
#    ylab = "Relative Sea Level (m)",
#    plot_tide_gauges = FALSE
#  )

## ----runigp, eval = TRUE------------------------------------------------------
res.eiv_igp_t <- reslr_mcmc(
  input_data =  CedarIslandNC_input,
  model_type = "eiv_igp_t",
  CI = "95%"
)

## ----printigpout, eval=TRUE---------------------------------------------------
print(res.eiv_igp_t)

## ----summaryigp, eval = TRUE--------------------------------------------------
summary(res.eiv_igp_t)

## ----runigpmore, eval = FALSE-------------------------------------------------
#  res.eiv_igp_t <- reslr_mcmc(
#    input_data =  CedarIslandNC_input,
#    model_type = "eiv_igp_t",
#    # Update these values
#    n_iterations = 6000,# Number of iterations
#    n_burnin = 1000,# Number of iterations to discard at the beginning
#    n_thin = 4,# Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#    )

## ----plotigpres, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.eiv_igp_t, plot_type = "model_fit_plot")

## ----plotigpresrate, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.eiv_igp_t,plot_type = "rate_plot")

## ----igpdataout, eval = TRUE--------------------------------------------------
output_dataframes <- res.eiv_igp_t$output_dataframes
head(output_dataframes)

## ---- dataexample3,eval = TRUE------------------------------------------------
# For 1 site
CedarIslandNC <- example_data_set %>% dplyr::filter(Site == "Cedar Island")

## ----loadspt, eval = TRUE-----------------------------------------------------
CedarIslandNC_input <- reslr_load(data = CedarIslandNC,
                                  include_tide_gauge = FALSE,
                                  include_linear_rate = FALSE,
                                  TG_minimum_dist_proxy = FALSE,
                                  list_preferred_TGs = NULL,
                                  all_TG_1deg = FALSE,
                                  prediction_interval = 50,
                                  rolling_window_average = 10)

## ----dataspt, eval=TRUE-------------------------------------------------------
data <- CedarIslandNC_input$data

## ----datagridspt, eval = TRUE-------------------------------------------------
data_grid <- CedarIslandNC_input$data_grid

## ----printspt, eval=TRUE------------------------------------------------------
print(CedarIslandNC_input)

## ----plotspt,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(CedarIslandNC_input)

## ----plotsptextra,fig.align = 'center',fig.width = 7,fig.height = 5,eval = FALSE----
#  plot(
#    x =  CedarIslandNC_input,
#    title = "Plot of the raw data",
#    xlab = "Age (CE)",
#    ylab = "Relative Sea Level (m)",
#    plot_tide_gauges = FALSE
#  )

## ----runspt,eval = TRUE-------------------------------------------------------
res.ni_spline_t <- reslr_mcmc(
  input_data =  CedarIslandNC_input,
  model_type = "ni_spline_t",
  CI = "95%")

## ----printresspt, eval=TRUE---------------------------------------------------
print(res.ni_spline_t)

## ----summaryspt, eval = TRUE--------------------------------------------------
summary(res.ni_spline_t)

## ---- runsptmore,eval = FALSE-------------------------------------------------
#  res.ni_spline_t <- reslr_mcmc(
#    input_data =  CedarIslandNC,
#    model_type = "ni_spline_t",
#    # Update these values
#    n_iterations = 6000,# Number of iterations
#    n_burnin = 1000,# Number of iterations to discard at the beginning
#    n_thin = 4,# Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#    )

## ---- plotsptres,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.ni_spline_t, 
     plot_type = "model_fit_plot")

## ----plotsptresrate, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.ni_spline_t,
     plot_type = "rate_plot")

## ----outdfspt, eval = TRUE----------------------------------------------------
output_dataframes <- res.ni_spline_t$output_dataframes
head(output_dataframes)

## ----data2sites, eval = TRUE--------------------------------------------------
# For 2 site
multi_site <- example_data_set %>% 
  dplyr::filter(Site %in% c("Cedar Island","Nassau"))

## ----loadspst, eval = TRUE----------------------------------------------------
multi_site_input <- reslr_load(data = multi_site,
                                  include_tide_gauge = FALSE,
                                  include_linear_rate = FALSE,
                                  TG_minimum_dist_proxy = FALSE,
                                  list_preferred_TGs = NULL,
                                  all_TG_1deg = FALSE,
                                  prediction_interval = 50,
                                  rolling_window_average = 10)

## ----dataspst,eval=TRUE-------------------------------------------------------
data <- multi_site_input$data
head(data)

## ----datagridspst, eval = TRUE------------------------------------------------
data_grid <- multi_site_input$data_grid
head(data_grid)

## ----printspst, eval=TRUE-----------------------------------------------------
print(multi_site_input)

## ----plotspst,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(multi_site_input)

## ----plotspstmore,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(
  x =  multi_site_input,
  title = "Plot of the raw data",
  xlab = "Age (CE)",
  ylab = "Relative Sea Level (m)",
  plot_tide_gauges = FALSE
)

## ----runspst,eval = TRUE------------------------------------------------------
res.ni_spline_st <- reslr_mcmc(
  input_data =  multi_site_input, 
  model_type = "ni_spline_st",
  CI = "95%")

## ----printspstout,eval=TRUE---------------------------------------------------
print(res.ni_spline_st)

## ----summaryspst, eval = TRUE-------------------------------------------------
summary(res.ni_spline_st)

## ---- runspstmore,eval = FALSE------------------------------------------------
#  res.ni_spline_st <- reslr::reslr_mcmc(
#    input_data =  multi_site_input,
#    model_type = "ni_spline_st",
#    # Update these values
#    n_iterations = 6000,# Number of iterations
#    n_burnin = 1000,# Number of iterations to discard at the beginning
#    n_thin = 4,# Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#    )

## ----plotspstres, eval = TRUE,fig.align = 'center',fig.width = 7,fig.height = 5----
plot(res.ni_spline_st, plot_type = "model_fit_plot")

## ----plotspstresrate, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.ni_spline_st,plot_type = "rate_plot")

## ----outdfspst, eval = TRUE---------------------------------------------------
output_dataframes <- res.ni_spline_st$output_dataframes
head(output_dataframes)

## ----data2sitesmore, eval = TRUE----------------------------------------------
# For 8 site
multi_8_sites <- example_data_set %>% 
  dplyr::filter(Site %in% c("Cedar Island","Nassau","Snipe Key",
                            "Placentia","Cape May Courthouse",
                            "Saint Simeon","Leeds Point","Wood Island"))

## ----loadnigam, eval = TRUE---------------------------------------------------
multi_8_sites_input <- reslr_load(data = multi_8_sites,
                                include_tide_gauge = TRUE,
                                include_linear_rate = TRUE,
                                TG_minimum_dist_proxy = FALSE,
                                list_preferred_TGs = NULL,
                                all_TG_1deg = TRUE,
                                prediction_interval = 50,
                                rolling_window_average = 10)

## ----datanigam,eval=TRUE------------------------------------------------------
data <- multi_8_sites_input$data

## ----datagridnigam, eval = TRUE-----------------------------------------------
data_grid <- multi_8_sites_input$data_grid

## ----printnigam, eval=TRUE----------------------------------------------------
print(multi_8_sites_input)

## ---- plotnigam,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(multi_8_sites_input)

## ----plotnigammore, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(
  x =  multi_8_sites_input,
  title = "Plot of the raw data",
  xlab = "Age (CE)",
  ylab = "Relative Sea Level (m)",
  plot_tide_gauges = TRUE
)

## ----runnigam,eval = TRUE-----------------------------------------------------
res.ni_gam_decomp <- reslr_mcmc(
  input_data =  multi_8_sites_input,
  model_type = "ni_gam_decomp",
  CI = "95%"
  )

## ----printnigamout, eval=TRUE-------------------------------------------------
print(res.ni_gam_decomp)

## ----summarynigam, eval = TRUE------------------------------------------------
summary(res.ni_gam_decomp)

## ---- runnigammore,eval = FALSE-----------------------------------------------
#  res.ni_gam_decomp <- reslr_mcmc(
#    input_data =  multi_8_sites_input,
#    model_type = "ni_gam_decomp",
#    # Update these values
#    n_iterations = 6000,# Number of iterations
#    n_burnin = 1000,# Number of iterations to discard at the beginning
#    n_thin = 4,# Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#    )

## ----plotnigamres, eval = TRUE,fig.align = 'center',fig.width = 7,fig.height = 5----
plot(res.ni_gam_decomp,
     plot_type = "model_fit_plot",
     plot_tide_gauge = FALSE)

## ----plotnigamresrate, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.ni_gam_decomp,
     plot_type = "rate_plot")

## ----totaldf, eval = TRUE-----------------------------------------------------
total_model_fit_df <- res.ni_gam_decomp$output_dataframes$total_model_fit_df
head(total_model_fit_df)

## ----totalrate, eval = TRUE---------------------------------------------------
total_model_rate_df <-res.ni_gam_decomp$output_dataframes$total_model_rate_df
head(total_model_rate_df)

## ----plotnigamregres, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.ni_gam_decomp,plot_type = "regional_plot")

## ----regdf, eval = FALSE------------------------------------------------------
#  regional_component_df <- res.ni_gam_decomp$output_dataframes$regional_component_df

## ----plotnigamregresrate, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.ni_gam_decomp,plot_type = "regional_rate_plot")

## ----regratedf, eval = FALSE--------------------------------------------------
#  regional_rate_component_df <-res.ni_gam_decomp$output_dataframes$regional_rate_component_df

## ----plotnigamlinres, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.ni_gam_decomp,plot_type = "linear_local_plot")

## ----linlocdf, eval = FALSE---------------------------------------------------
#  lin_loc_component_df <- res.ni_gam_decomp$output_dataframes$lin_loc_component_df

## ----linlocrate, eval = FALSE-------------------------------------------------
#  lin_loc_component_rates <- lin_loc_component_df %>%
#    dplyr::group_by(SiteName) %>%
#    dplyr::summarise(linear_rate = unique(linear_rate),
#                     linear_rate_err = unique(linear_rate_err))

## ----plotnigamnonlinres, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.ni_gam_decomp,plot_type = "non_linear_local_plot")

## ----non_lindf, eval = FALSE--------------------------------------------------
#  non_lin_loc_component_df <- res.ni_gam_decomp$output_dataframes$non_lin_loc_component_df

## ----plotnigamnonlinresrate, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.ni_gam_decomp,plot_type = "non_linear_local_rate_plot")

## ----nonlinratedf, eval = FALSE-----------------------------------------------
#  non_lin_loc_rate_component_df <-jags_output.ni_gam_decomp$output_dataframes$non_lin_loc_rate_component_df

## ----plotnigamall, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(res.ni_gam_decomp,plot_type = "nigam_component_plot")

## ---- eval = FALSE------------------------------------------------------------
#  multi_site <- reslr_load(data = multi_site,
#                                  include_tide_gauge = TRUE,
#                                  include_linear_rate = TRUE,
#                                  TG_minimum_dist_proxy = FALSE,
#                                  # There is no limit to the number of tide gauges provided in the list
#                                  list_preferred_TGs = c("ARGENTIA","MAYPORT",
#                                                    "JACKSONVILLE","LAKE WORTH PIER",
#                                                    "MAYPORT (BAR PILOTS DOCK), FLORIDA"),
#                                  all_TG_1deg = FALSE,
#                                  prediction_interval = 50,
#                                  rolling_window_average = 10)

## ----fig.align = 'center',fig.width = 7,fig.height = 5,eval = FALSE-----------
#  plot(
#    x =  multi_site,
#    title = "Plot of the raw data",
#    xlab = "Age (CE)",
#    ylab = "Relative Sea Level (m)",
#    plot_tide_gauges = TRUE
#  )

## ---- eval = FALSE------------------------------------------------------------
#  multi_site <- reslr_load(data = multi_site,
#                                  include_tide_gauge = TRUE,
#                                  include_linear_rate = TRUE,
#                                  TG_minimum_dist_proxy = FALSE,
#                                  list_preferred_TGs = NULL,
#                                  all_TG_1deg = TRUE,
#                                  prediction_interval = 50)

## ----fig.align = 'center',fig.width = 7,fig.height = 5,eval = FALSE-----------
#  plot(
#    x =  multi_site,
#    title = "Plot of the raw data",
#    xlab = "Age (CE)",
#    ylab = "Relative Sea Level (m)",
#    plot_tide_gauges = TRUE
#  )

## ---- eval = FALSE------------------------------------------------------------
#  # Example
#  final_plots <- plot(jags_output = reslr_mcmc(CedarIslandNC, model_type = "ni_spline_t"))
#  final_plots$plot_result
#  # Adding new title to the total model fit plot
#  final_plots$plot_result + ggplot2::ggtitle("New Title Added as Example")
#  final_plots$plot_result + ggplot2::xlab("New x axis label Added as Example")
#  final_plots$plot_result + ggplot2::ylab("New y axis label Added as Example")

## ---- eval = FALSE------------------------------------------------------------
#  # Example
#  res.eiv_slr_t <- reslr_mcmc(x =  CedarIslandNC, model_type = "eiv_slr_t")
#  res.eiv_slr_t$noisy_model_run_output$BUGSoutput$sims.list$beta

