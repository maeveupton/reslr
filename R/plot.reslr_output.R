#' Plotting the results for each statistical model. For the EIV simple linear regression and the Change point models the model fit plots are created, preferably for 1 proxy site.
#' For the EIV IGP and the NI spline in time the plots of the model fits and the derivative of the model fit are produced for 1 proxy site, as running these models with more than 1 site or with tide gauge data is not recommended.
#' For the NI spline in space time, the plots of the model fits and the derivatives of the model fits are produced for any amount of proxy sites.
#' For the NI GAM decomposition, the plots of the model fits and the derivatives of the model fits are produced for any amount of sites including tide gauges and proxy sites. It is important to note that tide gauge data is strongly recommended for this model. Also, the ni_gam_decomp model will produce plots for each individual component,i.e. the regional component and its rate of change, the linear local component, the non-linear local component and its rate of change.
#'
#' @param x An object of class \code{reslr_output} and \code{model_type} created via \code{\link{reslr_mcmc}}
#' @param plot_tide_gauges Plotting the tide gauge data as well as proxy data
#' @param ...  Not used
#'
#' @return Plot of model fit
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' x <- reslr_load(data = data)
#' jags_output <- reslr_mcmc(x, model_type = "eiv_slr_t")
#' plot(x =jags_output)
plot.reslr_output <- function(x,
                        plot_tide_gauges = TRUE,
                        ...) {
  Age <- RSL <-Age_err <- ID <- RSL_err <- lwr_95 <- upr_95 <- lwr_50 <- lwr_95 <- upr_50 <- rate_pred <- rate_lwr_95 <- rate_upr_95 <- rate_lwr_50 <- rate_upr_50 <- SiteName <- data_type_id <- pred <- NULL
  jags_output <- x
  # Not plotting the tide gauge data -------------
  # if (plot_tide_gauges == FALSE) {
  #   # Dataframes to plot
  #   output_dataframes <-
  #     jags_output$output_dataframes %>%
  #     dplyr::filter(data_type_id == "ProxyRecordData")
  #   data <- jags_output$data %>%
  #     dplyr::filter(data_type_id == "ProxyRecordData")
  #
  #   # jags_output$data <- jags_output$data %>%
  #   #   dplyr::mutate(data_type_id = as.factor(data_type_id)) %>%
  #   #   dplyr::filter(data_type_id == "ProxyRecordData")
  #   # jags_output$data_grid <- jags_output$data_grid %>%
  #   #   dplyr::mutate(data_type_id = as.factor(data_type_id)) %>%
  #   #   dplyr::filter(data_type_id == "ProxyRecordData")
  # }
  # else{
  #   # Dataframes to plot
  #   output_dataframes <- jags_output$output_dataframes
  #   data <- jags_output$data
  # }


  # EIV slr------------
  if (inherits(jags_output, "eiv_slr_t") == TRUE) {
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data

    # Plot
    plot_result <-
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
        ggplot2::aes(y = pred, ymin = lwr_95, ymax = upr_95, x = Age * 1000, fill = "95"), alpha = 0.2
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
                                   "95" = ggplot2::alpha("purple3", 0.2) #
                                   # "50" = ggplot2::alpha("purple3", 0.3)
                                 ),
                                 labels = c(
                                   "95% Credible Interval",
                                   expression(paste("1 sigma error"))
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
      )+
      ggplot2::facet_wrap(~SiteName)

    cat("Plotted EIV Simple linear regression. \n")

    output_plots <- list(plot_result = plot_result)
  }

  # EIV CP 1
  if (inherits(jags_output, "eiv_cp1_t")) {
    # Output from mcmc & dataframes for plots
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data

    # Plot
    plot_result <-
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
        ggplot2::aes(y = pred, ymin = lwr_95, ymax = upr_95, x = Age * 1000, fill = "95"), alpha = 0.2
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
                                   "95" = ggplot2::alpha("purple3", 0.2) #
                                   # "50" = ggplot2::alpha("purple3", 0.3)
                                 ),
                                 labels = c(
                                   "95% Credible Interval",
                                   expression(paste("1 sigma error"))
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
      )+
      ggplot2::facet_wrap(~SiteName)

    cat("Plotted EIV 1 Change Point model. \n")

    output_plots <- list(plot_result = plot_result)
  }


  # EIV CP2
  if (inherits(jags_output, "eiv_cp2_t")) {
    # Output from mcmc & dataframes for plots
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data

    # Plot
    plot_result <-
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
        ggplot2::aes(y = pred, ymin = lwr_95, ymax = upr_95, x = Age * 1000, fill = "95"), alpha = 0.2
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
                                   "95" = ggplot2::alpha("purple3", 0.2) #
                                   # "50" = ggplot2::alpha("purple3", 0.3)
                                 ),
                                 labels = c(
                                   "95% Credible Interval",
                                   expression(paste("1 sigma error"))
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
      )+
      ggplot2::facet_wrap(~SiteName)
    cat("Plotted 2 Change Point Model \n")

    output_plots <- list(plot_result = plot_result)
  }

  # EIV CP 3
  if (inherits(jags_output, "eiv_cp3_t")) {
    # Output from mcmc & dataframes for plots
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data
    # Plot
    plot_result <-
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
        ggplot2::aes(y = pred, ymin = lwr_95, ymax = upr_95, x = Age * 1000, fill = "95"), alpha = 0.2
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
                                   "95" = ggplot2::alpha("purple3", 0.2) #
                                   # "50" = ggplot2::alpha("purple3", 0.3)
                                 ),
                                 labels = c(
                                   "95% Credible Interval",
                                   expression(paste("1 sigma error"))
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
        )))+
      ggplot2::facet_wrap(~SiteName)

    cat("Plotted 3 Change Point \n")

    output_plots <- list(plot_result = plot_result)
  }

  # EIV IGP
  if (inherits(jags_output, "eiv_igp_t")) {
    # Output from mcmc & dataframes for plots
    data <- jags_output$data
    output_dataframes <- jags_output$output_dataframes

    # Plot
    plot_result <-
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
        ggplot2::aes(y = pred, ymin = lwr_95, ymax = upr_95, x = Age * 1000, fill = "95"), alpha = 0.2
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
                                   "95" = ggplot2::alpha("purple3", 0.2) #
                                   # "50" = ggplot2::alpha("purple3", 0.3)
                                 ),
                                 labels = c(
                                   "95% Credible Interval",
                                   expression(paste("1 sigma error"))
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
        )))+
      ggplot2::facet_wrap(~SiteName)

    cat("Plotted NI spline in time using data \n")

    # Plotting Rate of Change for Total component----------
    plot_rate <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = output_dataframes,
        ggplot2::aes(x = Age * 1000, y = rate_pred, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = output_dataframes,
        ggplot2::aes(y = rate_pred, ymin = rate_lwr_95, ymax = rate_upr_95, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = output_dataframes,
      #   ggplot2::aes(y = pred_rate, ymin = rate_lwr_50, ymax = rate_upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
      # ) +
      ggplot2::xlab("Age (CE)") +
      ggplot2::ylab("Rate of Change (mm/year)") +
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
                                   "95" = ggplot2::alpha("purple3", 0.2) #
                                   # "50" = ggplot2::alpha("purple3", 0.3)
                                 ),
                                 labels = c(
                                   "95% Credible Interval"
                                   # , "50% Credible Interval"
                                 )
      ) +
      ggplot2::scale_colour_manual("",
                                   values = c("mean" = "purple3"),
                                   labels = c( "Posterior Fit")
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(override.aes = list(
          alpha = c(0.4), # , 0.4),
          size = 1
        )),
        colour = ggplot2::guide_legend(override.aes = list(
          linetype = c(1),
          shape = c( NA),
          size = 2
        )))+
      ggplot2::facet_wrap(~SiteName)

    cat("Plotted EIV-IGP model & rate \n")
    output_plots <- list(plot_result = plot_result, plot_rate = plot_rate)
  }

  # NI spline time
  if (inherits(jags_output, "ni_spline_t") == TRUE) {
    # Dataframes to plot
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data

    # Plot
    plot_result <-
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
        ggplot2::aes(y = pred, ymin = lwr_95, ymax = upr_95, x = Age * 1000, fill = "95"), alpha = 0.2
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
                                   "95" = ggplot2::alpha("purple3", 0.2) #
                                   # "50" = ggplot2::alpha("purple3", 0.3)
                                 ),
                                 labels = c(
                                   "95% Credible Interval",
                                   expression(paste("1 sigma error"))
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
        )))+
      ggplot2::facet_wrap(~SiteName)

    cat("Plotted NI spline in time using data \n")

    # Plotting Rate of Change for Total component----------
    plot_rate <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = output_dataframes,
        ggplot2::aes(x = Age * 1000, y = rate_pred, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = output_dataframes,
        ggplot2::aes(y = rate_pred, ymin = rate_lwr_95, ymax = rate_upr_95, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = output_dataframes,
      #   ggplot2::aes(y = rate_pred, ymin = rate_lwr_50, ymax = rate_upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
      # ) +
      ggplot2::xlab("Age (CE)") +
      ggplot2::ylab("Rate of Change (mm/year)") +
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
                                   "95" = ggplot2::alpha("purple3", 0.2) #
                                   # "50" = ggplot2::alpha("purple3", 0.3)
                                 ),
                                 labels = c(
                                   "95% Credible Interval"
                                   # , "50% Credible Interval"
                                 )
      ) +
      ggplot2::scale_colour_manual("",
                                   values = c("mean" = "purple3"),
                                   labels = c( "Posterior Fit")
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(override.aes = list(
          alpha = c(0.4), # , 0.4),
          size = 1
        )),
        colour = ggplot2::guide_legend(override.aes = list(
          linetype = c(1),
          shape = c( NA),
          size = 2
        )))+
      ggplot2::facet_wrap(~SiteName)
    cat("Plotted rate of change for NI spline in time \n")


    output_plots <- list(
      plot_result = plot_result,
      plot_rate = plot_rate
    )
  }

  # NI spline space time
  if (inherits(jags_output, "ni_spline_st") == TRUE) {
    # Dataframes to plot
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data
    # Plot
    plot_result <-
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
        ggplot2::aes(y = pred, ymin = lwr_95, ymax = upr_95, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = output_dataframes,
      #   ggplot2::aes(y = pred, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
      # ) +
      ggplot2::xlab("Age (CE)") +
      ggplot2::ylab("Relative Sea Level (m)") +
      ggplot2::facet_wrap(~SiteName) +
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
                                   "95" = ggplot2::alpha("purple3", 0.2) #
                                   # "50" = ggplot2::alpha("purple3", 0.3)
                                 ),
                                 labels = c(
                                   "95% Credible Interval",
                                   expression(paste("1 sigma error"))
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
        )))

    cat("Plotted NI spline in time using data \n")

    # Plotting Rate of Change for Total component----------
    plot_rate <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = output_dataframes,
        ggplot2::aes(x = Age * 1000, y = rate_pred, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = output_dataframes,
        ggplot2::aes(y = rate_pred, ymin = rate_lwr_95, ymax = rate_upr_95, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = output_dataframes,
      #   ggplot2::aes(y = pred_rate, ymin = rate_lwr_50, ymax = rate_upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
      # ) +
      ggplot2::xlab("Age (CE)") +
      ggplot2::facet_wrap(~SiteName) +
      ggplot2::ylab("Rate of Change (mm/year)") +
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
                                   "95" = ggplot2::alpha("purple3", 0.2) #
                                   # "50" = ggplot2::alpha("purple3", 0.3)
                                 ),
                                 labels = c(
                                   "95% Credible Interval"
                                   # , "50% Credible Interval"
                                 )
      ) +
      ggplot2::scale_colour_manual("",
                                   values = c("mean" = "purple3"),
                                   labels = c( "Posterior Fit")
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(override.aes = list(
          alpha = c(0.4), # , 0.4),
          size = 1
        )),
        colour = ggplot2::guide_legend(override.aes = list(
          linetype = c(1),
          shape = c( NA),
          size = 2
        )))
    cat("NI spline in space time plotted and rate of change. \n")

    output_plots <- list(
      plot_result = plot_result,
      plot_rate = plot_rate
    )
  }

  # NIGAM decomposition
  if (inherits(jags_output, "ni_gam_decomp") == TRUE) {
    # Dataframes to plot
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data
    # Plots Total --------
    total_model_fit_df <-
      output_dataframes$total_model_fit_df

    plot_result<-
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
        data = total_model_fit_df,
        ggplot2::aes(x = Age * 1000, y = pred, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_fit_df,
        ggplot2::aes(y = pred, ymin = lwr_95, ymax = upr_95, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = total_model_fit_df,
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
                                   "95" = ggplot2::alpha("purple3", 0.2),
                                   "Uncertainty" = ggplot2::alpha("grey", 0.4)
                                   # "50" = ggplot2::alpha("purple3", 0.3)
                                 ),
                                 labels = c(
                                   "95% Credible Interval",
                                   expression(paste("1-sigma error"))
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
      ggplot2::facet_wrap(~SiteName)

    cat("Plotted the total model fit for the NIGAM decomposition \n")

    # Plotting Rate of Change for Total component
    total_model_rate_df <-   output_dataframes$total_model_rate_df
    plot_rate <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = total_model_rate_df,
        ggplot2::aes(x = Age * 1000, y = rate_pred, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_rate_df,
        ggplot2::aes(ymin = rate_lwr_95, ymax = rate_upr_95, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = total_model_rate_df,
        ggplot2::aes(ymin = rate_lwr_50, ymax = rate_upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
      ) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~SiteName) +
      ggplot2::ylab("Rate of change (mm/yr)") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 22),
        axis.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.text = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12)
      ) +
      ggplot2::theme(
        strip.text.x = ggplot2::element_text(size = 10),
        strip.background = ggplot2::element_rect(fill = c("white"))
      ) +
      ggplot2::xlab("Age (CE)") +
      ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
      ggplot2::labs(colour = "") +
      ggplot2::scale_fill_manual("",
                                 values = c(
                                   "95" = ggplot2::alpha("purple3", 0.2),
                                   "50" = ggplot2::alpha("purple3", 0.3)
                                 ),
                                 labels = c("95% Credible Interval", "50% Credible Interval")
      ) +
      ggplot2::scale_colour_manual("",
                                   values = c("mean" = "purple3"),
                                   labels = c("Posterior Fit")
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(override.aes = list(
          alpha = c(0.4, 0.2), # , 0.4),
          size = 1
        )),
        colour = ggplot2::guide_legend(override.aes = list(
          linetype = c(1),
          shape = c(NA),
          size = 2
        ))
      )
    cat("Plotted the rate of change for the total model fit for the NIGAM decomposition \n")


    # Regional Component Plot---------------------------
    regional_component_df <-  output_dataframes$regional_component_df
    regional_plot<-
      ggplot2::ggplot()+
      ggplot2::geom_line(data=regional_component_df,
                         ggplot2::aes(x=Age*1000,y=pred),colour="#3b47ad")+
      ggplot2::geom_ribbon(data=regional_component_df,
                           ggplot2::aes(ymin=lwr_95,ymax=upr_95,x=Age*1000),fill="#3b47ad",alpha=0.2)+
      ggplot2::geom_ribbon(data=regional_component_df,
                           ggplot2::aes(ymin=lwr_50,ymax=upr_50,x=Age*1000),fill="#3b47ad",alpha=0.3)+
      ggplot2::theme_bw()+
      ggplot2::theme(plot.title = ggplot2::element_text(size=15),
                     axis.title=ggplot2::element_text(size=12,face="bold"),
                     axis.text=ggplot2::element_text(size=12),
                     legend.text=ggplot2::element_text(size=12))+
      #ggplot2::ggtitle("Prediction")+
      ggplot2::ylab('Sea Level (m)')+
      ggplot2::xlab('Age (CE)')

    cat("Regional Component plotted \n")

    # Derivative of Predicted Regional Component Plot---------------------------
    regional_rate_component_df <-  output_dataframes$regional_rate_component_df
    regional_rate_plot<-
      ggplot2::ggplot()+
      ggplot2::geom_line(data=regional_rate_component_df,
                         ggplot2::aes(x=Age*1000,y=rate_pred),colour="#3b47ad")+
      ggplot2::geom_ribbon(data=regional_rate_component_df,
                           ggplot2::aes(ymin=rate_lwr_95,ymax=rate_upr_95,x=Age*1000),fill="#3b47ad",alpha=0.2)+
      ggplot2::geom_ribbon(data=regional_rate_component_df,
                           ggplot2::aes(ymin=rate_lwr_50,ymax=rate_upr_50,x=Age*1000),fill="#3b47ad",alpha=0.3)+
      ggplot2::theme_bw()+
      #ggplot2::ggtitle("Prediction Rate of Change")+
      ggplot2::theme(plot.title = ggplot2::element_text(size=15),
                     axis.title=ggplot2::element_text(size=12,face="bold"),
                     axis.text=ggplot2::element_text(size=12),
                     legend.text=ggplot2::element_text(size=12))+
      ggplot2::ylab('Rate of Change (mm/yr)')+
      ggplot2::xlab('Age (CE)')

    cat("Regional Rate Component plotted \n")


    # Linear Local + Site specific vertical offset ---------------------
    lin_loc_component_df <-  output_dataframes$lin_loc_component_df
    lin_loc_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = lin_loc_component_df,
        ggplot2::aes(x = Age * 1000, y = pred), colour = "#5bac06"
      ) +
      ggplot2::geom_ribbon(
        data = lin_loc_component_df,
        ggplot2::aes(ymin = lwr_95, ymax = upr_95, x = Age * 1000),
        fill = "#5bac06", alpha = 0.3
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 22),
        axis.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.text = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12)
      ) +
      ggplot2::theme(
        strip.text.x = ggplot2::element_text(size = 14),
        strip.background = ggplot2::element_rect(fill = c("white"))
      ) +
      ggplot2::ylab("Sea Level (m)") +
      ggplot2::facet_wrap(~SiteName) +
      ggplot2::xlab("Age (CE)")

    cat("Linear Local Component plotted \n")

    # Non-Linear Local Component: Spline in Space Time ----------------
    non_lin_loc_component_df <-  output_dataframes$non_lin_loc_component_df

    non_lin_loc_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = non_lin_loc_component_df,
        ggplot2::aes(x = Age * 1000, y = pred), colour = "#ad4c14"
      ) +
      ggplot2::geom_ribbon(
        data = non_lin_loc_component_df,
        ggplot2::aes(ymin = lwr_95, ymax = upr_95, x = Age * 1000), fill = "#ad4c14", alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = non_lin_loc_component_df,
        ggplot2::aes(ymin = lwr_50, ymax = upr_50, x = Age * 1000), fill = "#ad4c14", alpha = 0.3
      ) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::theme_bw() +
      ggplot2::ylab("Sea Level (m)") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 22),
        axis.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.text = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12)
      ) +
      ggplot2::facet_wrap(~SiteName) +
      ggplot2::theme(
        strip.text.x = ggplot2::element_text(size = 10),
        strip.background = ggplot2::element_rect(fill = c("white"))
      ) +
      ggplot2::xlab("Age (CE)")
    cat("Non-Linear Local Component Plot \n")

    # Non-Linear Local Component: Rate of change for Spline in Space Time --------------
    non_lin_loc_rate_component_df <-   output_dataframes$non_lin_loc_rate_component_df
    non_lin_loc_rate_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = non_lin_loc_rate_component_df,
        ggplot2::aes(x = Age * 1000, y = rate_pred), colour = "#ad4c14"
      ) +
      ggplot2::geom_ribbon(
        data = non_lin_loc_rate_component_df,
        ggplot2::aes(ymin = rate_lwr_95, ymax = rate_upr_95, x = Age * 1000), fill = "#ad4c14", alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = non_lin_loc_rate_component_df,
        ggplot2::aes(ymin = rate_lwr_50, ymax = rate_upr_50, x = Age * 1000), fill = "#ad4c14", alpha = 0.3
      ) +
      ggplot2::theme_bw() +
      ggplot2::ylab("Rate of Change (mm/year)") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 22),
        axis.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.text = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12)
      ) +
      ggplot2::facet_wrap(~SiteName) +
      ggplot2::theme(
        strip.text.x = ggplot2::element_text(size = 10),
        strip.background = ggplot2::element_rect(fill = c("white"))
      ) +
      ggplot2::xlab("Age (CE)")

    cat("Rate of change of Non-Linear Local Component Plot \n")

    # Separate Components on one plot with CI --------
    all_components_CI_plot <- ggplot2::ggplot() +
      # Linear Local Component + site specific vertical offset
      ggplot2::geom_line(
        data = lin_loc_component_df,
        ggplot2::aes(x = Age * 1000, y = pred, colour = ID)
      ) +
      ggplot2::geom_ribbon(
        data = lin_loc_component_df,
        ggplot2::aes(ymin = lwr_95, ymax = upr_95, x = Age * 1000, fill = ID), alpha = 0.3
      ) +
      # Local
      ggplot2::geom_line(
        data = non_lin_loc_component_df,
        ggplot2::aes(x = Age * 1000, y = pred, colour = ID)
      ) +
      ggplot2::geom_ribbon(
        data = non_lin_loc_component_df,
        ggplot2::aes(ymin = lwr_95, ymax = upr_95, x = Age * 1000, fill = ID), alpha = 0.3
      ) +

      # Regional Component
      ggplot2::geom_line(
        data = regional_component_df,
        ggplot2::aes(x = Age * 1000, y = pred, colour = ID)
      ) +
      ggplot2::geom_ribbon(
        data = regional_component_df,
        ggplot2::aes(ymin = lwr_95, ymax = upr_95, x = Age * 1000, fill = ID), alpha = 0.3
      ) +
      # Total Model
      ggplot2::geom_line(
        data = total_model_fit_df,
        ggplot2::aes(x = Age * 1000, y = pred, colour = ID)
      ) +
      ggplot2::geom_ribbon(
        data = total_model_fit_df,
        ggplot2::aes(ymin = lwr_95, ymax = upr_95, x = Age * 1000, fill = ID), alpha = 0.3
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        strip.text.x = ggplot2::element_text(size = 9),
        strip.background = ggplot2::element_rect(fill = c("white"))
      ) +
      ggplot2::scale_fill_manual(
        name = "", values = c("#5bac06", "#ad4c14", "#3b47ad", "purple3"),
        guide = ggplot2::guide_legend(override.aes = list(alpha = 0.1))
      ) +
      ggplot2::scale_colour_manual(name = "",
                                   values = c("#5bac06", "#ad4c14", "#3b47ad", "purple3")) +
      ggplot2::ylab("Sea Level (m)") +
      ggplot2::facet_wrap(~SiteName) +
      ggplot2::xlab("Age (CE)") +
      ggplot2::theme(legend.box = "horizontal", legend.position = "bottom")
      # ggplot2::theme(
      #   legend.position = c(0.95, -0.05),
      #   legend.justification = c(1, 0),
      #   legend.spacing.y = ggplot2::unit(0.1, "cm"),
      #   legend.title = ggplot2::element_blank(),
      #   legend.margin = ggplot2::margin(c(1, 1, 1, 1))
      # )

    cat("All Components plotted \n")

    output_plots <- list(
      plot_result = plot_result,
      plot_rate = plot_rate,
      regional_plot = regional_plot,
      regional_rate_plot = regional_rate_plot,
      lin_loc_plot = lin_loc_plot,
      non_lin_loc_plot = non_lin_loc_plot,
      non_lin_loc_rate_plot = non_lin_loc_rate_plot,
      all_components_CI_plot = all_components_CI_plot)
  }

  return(output_plots)
}
