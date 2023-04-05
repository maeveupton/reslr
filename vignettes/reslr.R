## ---- include = FALSE---------------------------------------------------------
# WHATS this
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "vig/"
)
options(rmarkdown.html_vignette.check_title = FALSE)
library(tidyverse)

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
CedarIslandNC <- reslr_load(data = CedarIslandNC,
                                  include_tide_gauge = FALSE,
                                  include_linear_rate = FALSE,
                                  TG_minimum_dist_proxy = FALSE,
                                  list_preferred_TGs = NULL,
                                  all_TG_1deg = FALSE,
                                  n_prediction = 100,
                                  input_Age_type = "CE")

## ---- slrdata,eval=TRUE-------------------------------------------------------
data <- CedarIslandNC$data

## ---- datagridslr, eval = TRUE------------------------------------------------
data_grid <- CedarIslandNC$data_grid

## ---- printdata, eval=TRUE----------------------------------------------------
print(CedarIslandNC)

## ---- plotdata,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(CedarIslandNC)

## ----plotdataextended,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(
  x = CedarIslandNC,
  title = "Plot of the raw data",
  xlab = "Age (CE)",
  ylab = "Relative Sea Level (m)",
  plot_tide_gauges = FALSE
)

## ---- runslr,eval = TRUE------------------------------------------------------
jags_output.eiv_slr_t <- reslr::reslr_mcmc(
  input_data =  CedarIslandNC,
  model_type = "eiv_slr_t"
)

## ---- printrunslr,eval=TRUE---------------------------------------------------
print(jags_output.eiv_slr_t)

## ---- summaryslr, eval = TRUE-------------------------------------------------
summary(jags_output.eiv_slr_t)

## ---- runslrmoreit,eval = FALSE-----------------------------------------------
#  jags_output.eiv_slr_t <- reslr::reslr_mcmc(
#    input_data =  CedarIslandNC,
#    model_type = "eiv_slr_t",
#    # Update these values
#    n_iterations = 6000,# Number of iterations
#    n_burnin = 1000,# Number of iterations to discard at the beginning
#    n_thin = 4,# Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#    )

## ---- plotslrres, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(x = jags_output.eiv_slr_t)

## ---- dataframeslrres, eval = TRUE--------------------------------------------
output_dataframes <- jags_output.eiv_slr_t$output_dataframes
head(output_dataframes)

## ----exampledata1, eval = TRUE------------------------------------------------
example_data_set <- reslr::NAACproxydata
# For 1 site
CedarIslandNC <- example_data_set %>% dplyr::filter(Site == "Cedar Island")

## ----loaddatacp, eval = TRUE--------------------------------------------------
CedarIslandNC <- reslr::reslr_load(data = CedarIslandNC,
                                  include_tide_gauge = FALSE,
                                  include_linear_rate = FALSE,
                                  TG_minimum_dist_proxy = FALSE,
                                  list_preferred_TGs = NULL,
                                  all_TG_1deg = FALSE,
                                  n_prediction = 100)

## ----datacp,eval=TRUE---------------------------------------------------------
data <- CedarIslandNC$data
head(data)

## ----datagridcp, eval = TRUE--------------------------------------------------
data_grid <- CedarIslandNC$data_grid
head(data_grid)

## ----printcp, eval=TRUE-------------------------------------------------------
print(CedarIslandNC)

## ----plotdatacp,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(x =  CedarIslandNC)

## ----plotdatacpmore,fig.align = 'center',fig.width = 7,fig.height = 5,eval = FALSE----
#  plot(
#    x =  CedarIslandNC,
#    title = "Plot of the raw data",
#    xlab = "Age (CE)",
#    ylab = "Relative Sea Level (m)",
#    plot_tide_gauges = FALSE
#  )

## ----runcp1, eval = TRUE------------------------------------------------------
jags_output.eiv_cp1_t <- reslr::reslr_mcmc(
  input_data  = CedarIslandNC,
  model_type = "eiv_cp_t", n_cp = 1
)

## ----runcp2, eval = TRUE------------------------------------------------------
jags_output.eiv_cp2_t <- reslr::reslr_mcmc(
  input_data =  CedarIslandNC,
  model_type = "eiv_cp_t", n_cp = 2
)

## ----printcpout, eval=TRUE----------------------------------------------------
print(jags_output.eiv_cp1_t)

## ----summarycp, eval = TRUE---------------------------------------------------
summary(jags_output.eiv_cp1_t)

## ----runcpmoreit,eval = FALSE-------------------------------------------------
#  jags_output.eiv_cp1_t <- reslr::reslr_mcmc(
#    input_data =  CedarIslandNC,
#    model_type = "eiv_cp_t",
#    # Update these values
#    n_iterations = 6000,# Number of iterations
#    n_burnin = 1000,# Number of iterations to discard at the beginning
#    n_thin = 4,# Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#    )

## ---- plotcpres, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(x = jags_output.eiv_cp1_t)

## ----cpdataframe, eval = TRUE-------------------------------------------------
output_dataframes <- jags_output.eiv_cp1_t$output_dataframes
head(output_dataframes)

## ----exampledataset2 ,eval = TRUE---------------------------------------------
example_data_set <- reslr::NAACproxydata
# For 1 site
CedarIslandNC <- example_data_set %>% dplyr::filter(Site == "Cedar Island")

## ---- loadigp ,eval = TRUE----------------------------------------------------
CedarIslandNC <- reslr::reslr_load(data = CedarIslandNC,
                                  include_tide_gauge = FALSE,
                                  include_linear_rate = FALSE,
                                  TG_minimum_dist_proxy = FALSE,
                                  list_preferred_TGs = NULL,
                                  all_TG_1deg = FALSE,
                                  n_prediction = 100)

## ----dataigp,eval=TRUE--------------------------------------------------------
data <- CedarIslandNC$data

## ----datagridigp, eval = TRUE-------------------------------------------------
data_grid <- CedarIslandNC$data_grid

## ----printigp, eval=TRUE------------------------------------------------------
print(CedarIslandNC)

## ---- plotigpdata,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(x =  CedarIslandNC)

## ---- plotigpdatamore,fig.align = 'center',fig.width = 7,fig.height = 5,eval = FALSE----
#  plot(
#    x =  CedarIslandNC,
#    title = "Plot of the raw data",
#    xlab = "Age (CE)",
#    ylab = "Relative Sea Level (m)",
#    plot_tide_gauges = FALSE
#  )

## ----runigp, eval = TRUE------------------------------------------------------
jags_output.eiv_igp_t <- reslr::reslr_mcmc(
  input_data =  CedarIslandNC,
  model_type = "eiv_igp_t"
)

## ----printigpout, eval=TRUE---------------------------------------------------
print(jags_output.eiv_igp_t)

## ----summaryigp, eval = TRUE--------------------------------------------------
summary(jags_output.eiv_igp_t)

## ----runigpmore, eval = FALSE-------------------------------------------------
#  jags_output.eiv_igp_t <- reslr::reslr_mcmc(
#    input_data =  CedarIslandNC,
#    model_type = "eiv_igp_t",
#    # Update these values
#    n_iterations = 6000,# Number of iterations
#    n_burnin = 1000,# Number of iterations to discard at the beginning
#    n_thin = 4,# Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#    )

## ----plotigpres, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(jags_output.eiv_igp_t)

## ----igpdataout, eval = TRUE--------------------------------------------------
output_dataframes <- jags_output.eiv_igp_t$output_dataframes
head(output_dataframes)

## ---- dataexample3,eval = TRUE------------------------------------------------
example_data_set <- reslr::NAACproxydata
# For 1 site
CedarIslandNC <- example_data_set %>% dplyr::filter(Site == "Cedar Island")

## ----loadspt, eval = TRUE-----------------------------------------------------
CedarIslandNC <- reslr::reslr_load(data = CedarIslandNC,
                                  include_tide_gauge = FALSE,
                                  include_linear_rate = FALSE,
                                  TG_minimum_dist_proxy = FALSE,
                                  list_preferred_TGs = NULL,
                                  all_TG_1deg = FALSE,
                                  n_prediction = 100)

## ----dataspt, eval=TRUE-------------------------------------------------------
data <- CedarIslandNC$data

## ----datagridspt, eval = TRUE-------------------------------------------------
data_grid <- CedarIslandNC$data_grid

## ----printspt, eval=TRUE------------------------------------------------------
print(CedarIslandNC)

## ----plotspt,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(x =  CedarIslandNC)

## ----plotsptextra,fig.align = 'center',fig.width = 7,fig.height = 5,eval = FALSE----
#  plot(
#    x =  CedarIslandNC,
#    title = "Plot of the raw data",
#    xlab = "Age (CE)",
#    ylab = "Relative Sea Level (m)",
#    plot_tide_gauges = FALSE
#  )

## ----runspt,eval = TRUE-------------------------------------------------------
jags_output.ni_spline_t <- reslr::reslr_mcmc(input_data =  CedarIslandNC,
                                   model_type = "ni_spline_t")

## ----printresspt, eval=TRUE---------------------------------------------------
print(jags_output.ni_spline_t)

## ----summaryspt, eval = TRUE--------------------------------------------------
summary(jags_output.ni_spline_t)

## ---- runsptmore,eval = FALSE-------------------------------------------------
#  jags_output.ni_spline_t <- reslr::reslr_mcmc(
#    input_data =  CedarIslandNC,
#    model_type = "ni_spline_t",
#    # Update these values
#    n_iterations = 6000,# Number of iterations
#    n_burnin = 1000,# Number of iterations to discard at the beginning
#    n_thin = 4,# Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#    )

## ---- plotsptres,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(jags_output.ni_spline_t)

## ----outdfspt, eval = TRUE----------------------------------------------------
output_dataframes <- jags_output.ni_spline_t$output_dataframes
head(output_dataframes)

## ----data2sites, eval = TRUE--------------------------------------------------
example_data_set <- reslr::NAACproxydata
# For 2 site
multi_site <- example_data_set %>% dplyr::filter(Site %in% c("Cedar Island","Nassau"))

## ----loadspst, eval = TRUE----------------------------------------------------
multi_site <- reslr::reslr_load(data = multi_site,
                                  include_tide_gauge = FALSE,
                                  include_linear_rate = FALSE,
                                  TG_minimum_dist_proxy = FALSE,
                                  list_preferred_TGs = NULL,
                                  all_TG_1deg = FALSE,
                                  n_prediction = 100)

## ----dataspst,eval=TRUE-------------------------------------------------------
data <- multi_site$data
head(data)

## ----datagridspst, eval = TRUE------------------------------------------------
data_grid <- multi_site$data_grid
head(data_grid)

## ----printspst, eval=TRUE-----------------------------------------------------
print(multi_site)

## ----plotspst,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(multi_site)

## ----plotspstmore,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(
  x =  multi_site,
  title = "Plot of the raw data",
  xlab = "Age (CE)",
  ylab = "Relative Sea Level (m)",
  plot_tide_gauges = FALSE
)

## ----runspst,eval = TRUE------------------------------------------------------
jags_output.ni_spline_st <- reslr::reslr_mcmc(input_data =  multi_site, 
                                              model_type = "ni_spline_st")

## ----printspstout,eval=TRUE---------------------------------------------------
print(jags_output.ni_spline_st)

## ----summaryspst, eval = TRUE-------------------------------------------------
summary(jags_output.ni_spline_st)

## ---- runspstmore,eval = FALSE------------------------------------------------
#  jags_output.ni_spline_st <- reslr::reslr_mcmc(
#    input_data =  multi_site,
#    model_type = "ni_spline_st",
#    # Update these values
#    n_iterations = 6000,# Number of iterations
#    n_burnin = 1000,# Number of iterations to discard at the beginning
#    n_thin = 4,# Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#    )

## ----plotspstres, eval = TRUE,fig.align = 'center',fig.width = 7,fig.height = 5----
plot(x = jags_output.ni_spline_st)

## ----outdfspst, eval = TRUE---------------------------------------------------
output_dataframes <- jags_output.ni_spline_st$output_dataframes
head(output_dataframes)

## ----data2sitesmore, eval = TRUE----------------------------------------------
example_data_set <- reslr::NAACproxydata
# For 2 site
multi_site <- example_data_set %>% dplyr::filter(Site %in% c("Cedar Island","Nassau","Snipe Key","Placentia"))

## ----loadnigam, eval = TRUE---------------------------------------------------
multi_site <- reslr::reslr_load(data = multi_site,
                                include_tide_gauge = TRUE,
                                include_linear_rate = TRUE,
                                TG_minimum_dist_proxy = TRUE,
                                list_preferred_TGs = NULL,
                                all_TG_1deg = FALSE,
                                n_prediction = 100)

## ----datanigam,eval=TRUE------------------------------------------------------
data <- multi_site$data

## ----datagridnigam, eval = TRUE-----------------------------------------------
data_grid <- multi_site$data_grid

## ----printnigam, eval=TRUE----------------------------------------------------
print(multi_site)

## ---- plotnigam,fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(x =  multi_site)

## ----plotnigammore, fig.align = 'center',fig.width = 7,fig.height = 5,eval = TRUE----
plot(
  x =  multi_site,
  title = "Plot of the raw data",
  xlab = "Age (CE)",
  ylab = "Relative Sea Level (m)",
  plot_tide_gauges = TRUE
)

## ----runnigam,eval = TRUE-----------------------------------------------------
jags_output.ni_gam_decomp <- reslr::reslr_mcmc(
  input_data =  multi_site,
  model_type = "ni_gam_decomp")

## ----printnigamout, eval=TRUE-------------------------------------------------
print(jags_output.ni_gam_decomp)

## ----summarynigam, eval = TRUE------------------------------------------------
summary(jags_output.ni_gam_decomp)

## ---- runnigammore,eval = FALSE-----------------------------------------------
#  jags_output.ni_gam_decomp <- reslr::reslr_mcmc(
#    input_data =  multi_site,
#    model_type = "ni_gam_decomp",
#    # Update these values
#    n_iterations = 6000,# Number of iterations
#    n_burnin = 1000,# Number of iterations to discard at the beginning
#    n_thin = 4,# Reduces number of output samples to save memory and computation time
#    n_chains = 3 # Number of Markov chains
#    )

## ----plotnigamres, eval = TRUE,fig.align = 'center',fig.width = 7,fig.height = 5----
plot(x = jags_output.ni_gam_decomp,plot_tide_gauge = FALSE)

## ----totaldf, eval = FALSE----------------------------------------------------
#  total_model_fit_df <- jags_output.ni_gam_decomp$output_dataframes$total_model_fit_df

## ----totalrate, eval = FALSE--------------------------------------------------
#  total_model_rate_df <-jags_output.ni_gam_decomp$output_dataframes$total_model_rate_df

## ----regdf, eval = FALSE------------------------------------------------------
#  regional_component_df <- jags_output.ni_gam_decomp$output_dataframes$regional_component_df

## ----regratedf, eval = FALSE--------------------------------------------------
#  regional_rate_component_df <-jags_output.ni_gam_decomp$output_dataframes$regional_rate_component_df

## ----linlocdf, eval = FALSE---------------------------------------------------
#  lin_loc_component_df <- jags_output.ni_gam_decomp$output_dataframes$lin_loc_component_df

## ----linlocrate, eval = FALSE-------------------------------------------------
#  lin_loc_component_rates <- lin_loc_component_df %>%
#    dplyr::group_by(SiteName) %>%
#    dplyr::summarise(linear_rate = unique(linear_rate),
#                     linear_rate_err = unique(linear_rate_err))

## ----non_lindf, eval = FALSE--------------------------------------------------
#  non_lin_loc_component_df <- jags_output.ni_gam_decomp$output_dataframes$non_lin_loc_component_df

## ----nonlinratedf, eval = FALSE-----------------------------------------------
#  non_lin_loc_rate_component_df <-jags_output.ni_gam_decomp$output_dataframes$non_lin_loc_rate_component_df

## ---- eval = FALSE------------------------------------------------------------
#  multi_site <- reslr::reslr_load(data = multi_site,
#                                  include_tide_gauge = TRUE,
#                                  include_linear_rate = TRUE,
#                                  TG_minimum_dist_proxy = FALSE,
#                                  # There is no limit to the number of tide gauges provided in the list
#                                  list_preferred_TGs = c("ARGENTIA","MAYPORT",
#                                                    "JACKSONVILLE","LAKE WORTH PIER",
#                                                    "MAYPORT (BAR PILOTS DOCK), FLORIDA"),
#                                  all_TG_1deg = FALSE,
#                                  n_prediction = 100)

## ----fig.align = 'center',fig.width = 7,fig.height = 5,eval = FALSE-----------
#  plot(
#    x =  multi_site,
#    title = "Plot of the raw data",
#    xlab = "Age (CE)",
#    ylab = "Relative Sea Level (m)",
#    plot_tide_gauges = TRUE
#  )

## ---- eval = FALSE------------------------------------------------------------
#  multi_site <- reslr::reslr_load(data = multi_site,
#                                  include_tide_gauge = TRUE,
#                                  include_linear_rate = TRUE,
#                                  TG_minimum_dist_proxy = FALSE,
#                                  list_preferred_TGs = NULL,
#                                  all_TG_1deg = TRUE,
#                                  n_prediction = 100)

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
#  final_plots <- plot(jags_output = reslr::reslr_mcmc(x =  CedarIslandNC, model_type = "ni_spline_t"))
#  final_plots$plot_result
#  # Adding new title to the total model fit plot
#  final_plots$plot_result + ggplot2::ggtitle("New Title Added as Example")
#  final_plots$plot_result + ggplot2::xlab("New x axis label Added as Example")
#  final_plots$plot_result + ggplot2::ylab("New y axis label Added as Example")

## ---- eval = FALSE------------------------------------------------------------
#  # Example
#  jags_output.eiv_slr_t <- reslr::reslr_mcmc(x =  CedarIslandNC, model_type = "eiv_slr_t")
#  jags_output.eiv_slr_t$noisy_model_run_output$BUGSoutput$sims.list$beta

