#' Plotting the results for each statistical model from the \code{reslr_mcmc} function.
#'
#' For the EIV simple linear regression and the Change point models the model fit plots are created, preferably for 1 proxy site.
#' For the EIV IGP and the NI spline in time the plots of the model fits and the derivative of the model fit are produced for 1 proxy site, as running these models with more than 1 site or with tide gauge data is not recommended.
#' For the NI spline in space time, the plots of the model fits and the derivatives of the model fits are produced for any amount of proxy sites.
#' For the NI GAM decomposition, the plots of the model fits and the derivatives of the model fits are produced for any amount of sites including tide gauges and proxy sites. It is important to note that tide gauge data is strongly recommended for this model. Also, the ni_gam_decomp model will produce plots for each individual component,i.e. the regional component and its rate of change, the linear local component, the non-linear local component and its rate of change.
#' If tide gauges are used in the model, the user has the ability plot the output with or without the inclusion of the tide gauges.
#'
#' @param x An object of class \code{reslr_output} and \code{model_type} created via \code{\link{reslr_mcmc}}
#' @param plot_proxy_records Plotting the proxy records on their own and this is the default
#' @param plot_tide_gauges Plotting the tide gauge data as well as proxy data
#' @param plot_type The user can select the type of output plot they require from the following: "rate_plot","model_fit_plot","regional_plot","regional_rate_plot","linear_local_plot","non_linear_local_plot","non_linear_local_rate_plot","nigam_component_plot"
#' @param plot_caption Plotting an informed caption with the number of tide gauges and proxy sites.
#' @param xlab Labeling the x-axis
#' @param ylab Labeling the y-axis
#' @param title Plotting a title on the output plots
#' @param y_rate_lab Labeling the y-axis for rate of change plots
#' @param ...  Not used
#'
#' @return Plot of model fit and the rate of change depending on the statistical model in question.
#' @export
#'
#' @examples
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' x <- reslr_load(data = data)
#' jags_output <- reslr_mcmc(x, model_type = "eiv_slr_t")
#' plot(x = jags_output)
plot.reslr_output <- function(x,
                              plot_proxy_records = TRUE,
                              plot_tide_gauges = FALSE,
                              title = "",
                              plot_type = c("model_fit_plot"),
                              plot_caption = TRUE,
                              xlab = "Year (CE)",
                              ylab = "Relative Sea Level (m)",
                              y_rate_lab = "Rate of change (mm/year)",
                              ...) {
  Age <- RSL <- Age_err <- ID <- RSL_err <- lwr <- upr <- lwr <- rate_pred <- rate_lwr <- rate_upr <- SiteName <- data_type_id <- pred <- NULL
  jags_output <- x

  # EIV slr------------
  if (inherits(jags_output, "eiv_slr_t") == TRUE) {
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data
    n_sites <- length(data$SiteName %>% unique())
    n_proxy <- data %>%
      dplyr::filter(data_type_id == "ProxyRecord") %>%
      dplyr::select(SiteName, data_type_id) %>%
      unique() %>%
      nrow()
    n_tide_gauges <- n_sites - n_proxy
    # Plotting proxy only
    if (plot_proxy_records == TRUE & plot_tide_gauges == FALSE) {
      output_dataframes <- output_dataframes %>%
        dplyr::filter(data_type_id == "ProxyRecord")
      data <- data %>%
        dplyr::filter(data_type_id == "ProxyRecord")
    }
    # Plotting tide gauge only
    if (plot_proxy_records == FALSE & plot_tide_gauges == TRUE) {
      output_dataframes <- output_dataframes %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      data <- data %>%
        dplyr::filter(data_type_id == "TideGaugeData")
    }

    # Using caption or not
    if (plot_caption == TRUE) {
      plot_result <- create_model_fit_plot(
        output_dataframes = output_dataframes,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = paste0(
          "Model type: Errors in Variables Simple Linear Regression \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_tide_gauges
        )
      )
    } else {
      plot_result <- create_model_fit_plot(
        output_dataframes = output_dataframes,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = NULL
      )
    }
    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result + ggplot2::facet_wrap(~SiteName, scales = "free")
    }
    output_plots <- list(plot_result = plot_result)
  }

  # EIV CP 1-------------
  if (inherits(jags_output, "eiv_cp1_t")) {
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data
    n_sites <- length(data$SiteName %>% unique())
    n_proxy <- data %>%
      dplyr::filter(data_type_id == "ProxyRecord") %>%
      dplyr::select(SiteName, data_type_id) %>%
      unique() %>%
      nrow()
    n_tide_gauges <- n_sites - n_proxy
    # Plotting proxy only
    if (plot_proxy_records == TRUE & plot_tide_gauges == FALSE) {
      output_dataframes <- output_dataframes %>%
        dplyr::filter(data_type_id == "ProxyRecord")
      data <- data %>%
        dplyr::filter(data_type_id == "ProxyRecord")
    }
    # Plotting tide gauge only
    if (plot_proxy_records == FALSE & plot_tide_gauges == TRUE) {
      output_dataframes <- output_dataframes %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      data <- data %>%
        dplyr::filter(data_type_id == "TideGaugeData")
    }

    # Using caption or not
    if (plot_caption == TRUE) {
      plot_result <- create_model_fit_plot(
        output_dataframes = output_dataframes,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = paste0(
          "Model type: Errors in Variables 1 Change Point Model \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        )
      )
    } else {
      plot_result <- create_model_fit_plot(
        output_dataframes = output_dataframes,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = NULL
      )
    }
    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result + ggplot2::facet_wrap(~SiteName, scales = "free")
    }
    output_plots <- list(plot_result = plot_result)
  }


  # EIV CP2------------
  if (inherits(jags_output, "eiv_cp2_t")) {
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data
    n_sites <- length(data$SiteName %>% unique())
    n_proxy <- data %>%
      dplyr::filter(data_type_id == "ProxyRecord") %>%
      dplyr::select(SiteName, data_type_id) %>%
      unique() %>%
      nrow()
    n_tide_gauges <- n_sites - n_proxy
    # Plotting proxy only
    if (plot_proxy_records == TRUE & plot_tide_gauges == FALSE) {
      output_dataframes <- output_dataframes %>%
        dplyr::filter(data_type_id == "ProxyRecord")
      data <- data %>%
        dplyr::filter(data_type_id == "ProxyRecord")
    }
    # Plotting tide gauge only
    if (plot_proxy_records == FALSE & plot_tide_gauges == TRUE) {
      output_dataframes <- output_dataframes %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      data <- data %>%
        dplyr::filter(data_type_id == "TideGaugeData")
    }

    # Using caption or not
    if (plot_caption == TRUE) {
      plot_result <- create_model_fit_plot(
        output_dataframes = output_dataframes,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = paste0(
          "Model type: Errors in Variables 2 Change Point Model \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        )
      )
    } else {
      plot_result <- create_model_fit_plot(
        output_dataframes = output_dataframes,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = NULL
      )
    }
    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result + ggplot2::facet_wrap(~SiteName, scales = "free")
    }
    output_plots <- list(plot_result = plot_result)
  }

  # EIV CP 3
  if (inherits(jags_output, "eiv_cp3_t")) {
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data
    n_sites <- length(data$SiteName %>% unique())
    n_proxy <- data %>%
      dplyr::filter(data_type_id == "ProxyRecord") %>%
      dplyr::select(SiteName, data_type_id) %>%
      unique() %>%
      nrow()
    n_tide_gauges <- n_sites - n_proxy
    # Plotting proxy only
    if (plot_proxy_records == TRUE & plot_tide_gauges == FALSE) {
      output_dataframes <- output_dataframes %>%
        dplyr::filter(data_type_id == "ProxyRecord")
      data <- data %>%
        dplyr::filter(data_type_id == "ProxyRecord")
    }
    # Plotting tide gauge only
    if (plot_proxy_records == FALSE & plot_tide_gauges == TRUE) {
      output_dataframes <- output_dataframes %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      data <- data %>%
        dplyr::filter(data_type_id == "TideGaugeData")
    }

    # Using caption or not
    if (plot_caption == TRUE) {
      plot_result <- create_model_fit_plot(
        output_dataframes = output_dataframes,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = paste0(
          "Model type: Errors in Variables 3 Change Point Model \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        )
      )
    } else {
      plot_result <- create_model_fit_plot(
        output_dataframes = output_dataframes,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = NULL
      )
    }
    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result + ggplot2::facet_wrap(~SiteName, scales = "free")
    }
    output_plots <- list(plot_result = plot_result)
  }

  # EIV IGP---------------------
  if (inherits(jags_output, "eiv_igp_t")) {
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data
    n_sites <- length(data$SiteName %>% unique())
    n_proxy <- data %>%
      dplyr::filter(data_type_id == "ProxyRecord") %>%
      dplyr::select(SiteName, data_type_id) %>%
      unique() %>%
      nrow()
    n_tide_gauges <- n_sites - n_proxy

    # Plotting the detrended data:
    if (inherits(jags_output, "data_detrend") == TRUE) {
      # Plotting proxy only
      if (plot_proxy_records == TRUE & plot_tide_gauges == FALSE) {
        output_dataframes <- output_dataframes %>%
          dplyr::filter(data_type_id == "ProxyRecord")
        data <- data %>%
          dplyr::filter(data_type_id == "ProxyRecord")
      }
      # Plotting tide gauge only
      if (plot_proxy_records == FALSE & plot_tide_gauges == TRUE) {
        output_dataframes <- output_dataframes %>%
          dplyr::filter(data_type_id == "TideGaugeData")
        data <- data %>%
          dplyr::filter(data_type_id == "TideGaugeData")
      }
      # Using caption or not
      if (plot_caption == TRUE) {
        # Plot model fit
        plot_result <- create_model_fit_plot(
          output_dataframes = output_dataframes,
          data = data,
          xlab = xlab,
          ylab = ylab,
          title = title,
          model_caption = paste0(
            "Model type: Errors in Variables Integrated Gaussian Process Model \n No. proxy sites:", n_proxy,
            "\n No. tide gauge sites:", n_sites - n_proxy
          )
        )

        # Plot rate
        plot_rate <- create_rate_of_change_plot(
          output_dataframes = output_dataframes,
          data = data,
          model_caption = paste0(
            "Model type: Errors in Variables Integrated Gaussian Process Model \n No. proxy sites:", n_proxy,
            "\n No. tide gauge sites:", n_sites - n_proxy
          ),
          xlab = xlab,
          y_rate_lab = y_rate_lab,
          title = title
        )
      } else {
        # Plot model fit
        plot_result <- create_model_fit_plot(
          output_dataframes = output_dataframes,
          data = data,
          xlab = xlab,
          ylab = ylab,
          title = title,
          model_caption = NULL
        )

        # Plot rate
        plot_rate <- create_rate_of_change_plot(
          output_dataframes = output_dataframes,
          data = data,
          model_caption = NULL,
          xlab = xlab,
          y_rate_lab = y_rate_lab,
          title = title
        )
      }
      if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
        plot_result <- plot_result +
          ggplot2::facet_wrap(~SiteName, scales = "free")
        plot_rate <- plot_rate +
          ggplot2::facet_wrap(~SiteName, scales = "free")
      }


    }


    # Plot the normal data:
    else {
      # Plotting proxy only
      if (plot_proxy_records == TRUE & plot_tide_gauges == FALSE) {
        output_dataframes <- output_dataframes %>%
          dplyr::filter(data_type_id == "ProxyRecord")
        data <- data %>%
          dplyr::filter(data_type_id == "ProxyRecord")
      }
      # Plotting tide gauge only
      if (plot_proxy_records == FALSE & plot_tide_gauges == TRUE) {
        output_dataframes <- output_dataframes %>%
          dplyr::filter(data_type_id == "TideGaugeData")
        data <- data %>%
          dplyr::filter(data_type_id == "TideGaugeData")
      }

      # Using caption or not
      if (plot_caption == TRUE) {
        # Plot model fit
        plot_result <- create_model_fit_plot(
          output_dataframes = output_dataframes,
          data = data,
          xlab = xlab,
          ylab = ylab,
          title = title,
          model_caption = paste0(
            "Model type: Errors in Variables Integrated Gaussian Process Model \n No. proxy sites:", n_proxy,
            "\n No. tide gauge sites:", n_sites - n_proxy
          )
        )

        # Plot rate
        plot_rate <- create_rate_of_change_plot(
          output_dataframes = output_dataframes,
          data = data,
          model_caption = paste0(
            "Model type: Errors in Variables Integrated Gaussian Process Model \n No. proxy sites:", n_proxy,
            "\n No. tide gauge sites:", n_sites - n_proxy
          ),
          xlab = xlab,
          y_rate_lab = y_rate_lab,
          title = title
        )
      } else {
        # Plot model fit
        plot_result <- create_model_fit_plot(
          output_dataframes = output_dataframes,
          data = data,
          xlab = xlab,
          ylab = ylab,
          title = title,
          model_caption = NULL
        )

        # Plot rate
        plot_rate <- create_rate_of_change_plot(
          output_dataframes = output_dataframes,
          data = data,
          model_caption = NULL,
          xlab = xlab,
          y_rate_lab = y_rate_lab,
          title = title
        )
      }
      if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
        plot_result <- plot_result +
          ggplot2::facet_wrap(~SiteName, scales = "free")
        plot_rate <- plot_rate +
          ggplot2::facet_wrap(~SiteName, scales = "free")
      }
    }

    # # Not working yet
    # if (inherits(jags_output, "data_detrend") == TRUE) {
    #   # Plot result with informed caption
    #   if (plot_caption == TRUE) {
    #     # Plot
    #     plot <-
    #       ggplot2::ggplot() +
    #       ggplot2::geom_rect(data = data, ggplot2::aes(
    #         xmin = Age - Age_err,
    #         xmax = Age + Age_err,
    #         ymin = y_lwr_box,
    #         ymax = y_upr_box,
    #         fill = "Uncertainty",
    #       ), alpha = 0.7) +
    #       ggplot2::geom_point(
    #         data = data,
    #         ggplot2::aes(y = SL, x = Age, colour = "black"), size = 0.3
    #       ) +
    #       ggplot2::geom_line(
    #         data = output_dataframes,
    #         ggplot2::aes(x = Age, y = pred, colour = "mean")
    #       ) +
    #       ggplot2::geom_ribbon(
    #         data = output_dataframes,
    #         ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age, fill = "CI"), alpha = 0.2
    #       ) +
    #       # ggplot2::labs(x = xlab, y = ylab, title = title) #+
    #       ggplot2::labs(x = "Time (CE)", y = "Sea Level (m)", title = "", colour = "") +
    #       ggplot2::theme_bw() +
    #       ggplot2::theme(
    #         plot.title = ggplot2::element_text(size = 15),
    #         axis.title = ggplot2::element_text(size = 12, face = "bold"),
    #         axis.text = ggplot2::element_text(size = 12),
    #         legend.text = ggplot2::element_text(size = 10)
    #       ) +
    #       ggplot2::theme(
    #         strip.text.x = ggplot2::element_text(size = 10),
    #         strip.background = ggplot2::element_rect(fill = c("white"))
    #       ) +
    #       ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
    #       ggplot2::scale_fill_manual("",
    #         values = c(
    #           "Uncertainty" = ggplot2::alpha("grey", 0.3),
    #           "CI" = ggplot2::alpha("purple3", 0.2)
    #         ),
    #         labels = c(
    #           CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
    #           expression(paste("1-sigma Error"))
    #         )
    #       ) +
    #       ggplot2::scale_colour_manual("",
    #         values = c("black" = "black", "mean" = "purple3"),
    #         labels = c("Data", "Posterior Fit")
    #       ) +
    #       ggplot2::guides(
    #         fill = ggplot2::guide_legend(override.aes = list(
    #           alpha = c(0.4, 0.2), # , 0.4),
    #           size = 1
    #         )),
    #         colour = ggplot2::guide_legend(override.aes = list(
    #           linetype = c(0, 1),
    #           shape = c(16, NA),
    #           size = 2
    #         ))
    #       ) +
    #       ggplot2::facet_wrap(~SiteName) #+
    #     # ggplot2::labs(caption = paste0(
    #     #  "Model type: Errors in Variables Integrated Gaussian Process \n No. proxy sites:", n_proxy,
    #     #  "\n No. tide gauge sites:", n_sites - n_proxy
    #     # ))
    #     # Plotting Rate of Change for Total component----------
    #     plot_rate <-
    #       ggplot2::ggplot() +
    #       ggplot2::geom_line(
    #         data = output_dataframes,
    #         ggplot2::aes(x = Age, y = rate_pred, colour = "mean")
    #       ) +
    #       ggplot2::geom_ribbon(
    #         data = output_dataframes,
    #         ggplot2::aes(y = rate_pred, ymin = rate_lwr, ymax = rate_upr, x = Age, fill = "CI"), alpha = 0.2
    #       ) +
    #       # ggplot2::labs(x = xlab, y = y_rate_lab, title = title,colour = "") +
    #       ggplot2::labs(x = "Time (CE)", y = "Sea Level (m)", title = "", colour = "") +
    #       ggplot2::theme_bw() +
    #       ggplot2::theme(
    #         plot.title = ggplot2::element_text(size = 15),
    #         axis.title = ggplot2::element_text(size = 12, face = "bold"),
    #         axis.text = ggplot2::element_text(size = 12),
    #         legend.text = ggplot2::element_text(size = 10)
    #       ) +
    #       ggplot2::theme(
    #         strip.text.x = ggplot2::element_text(size = 10),
    #         strip.background = ggplot2::element_rect(fill = c("white"))
    #       ) +
    #       ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
    #       ggplot2::scale_fill_manual("",
    #         values = c(
    #           "CI" = ggplot2::alpha("purple3", 0.2)
    #         ),
    #         labels = c(
    #           CI = paste0(unique(output_dataframes$CI), " Credible Interval")
    #         )
    #       ) +
    #       ggplot2::scale_colour_manual("",
    #         values = c("mean" = "purple3"),
    #         labels = c("Posterior Fit")
    #       ) +
    #       ggplot2::geom_hline(yintercept = 0) +
    #       ggplot2::guides(
    #         fill = ggplot2::guide_legend(override.aes = list(
    #           alpha = c(0.4), # , 0.4),
    #           size = 1
    #         )),
    #         colour = ggplot2::guide_legend(override.aes = list(
    #           linetype = c(1),
    #           shape = c(NA),
    #           size = 2
    #         ))
    #       ) +
    #       ggplot2::facet_wrap(~SiteName) #+
    #     # ggplot2::labs(caption = paste0(
    #     #   "Model type: Errors in Variables Integrated Gaussian Process \n No. proxy sites:", n_proxy,
    #     #   "\n No. tide gauge sites:", n_sites - n_proxy
    #     # ))
    #   } else {
    #     # Plot
    #     plot <-
    #       ggplot2::ggplot() +
    #       ggplot2::geom_rect(data = data, ggplot2::aes(
    #         xmin = Age - Age_err,
    #         xmax = Age + Age_err,
    #         ymin = y_lwr_box,
    #         ymax = y_upr_box,
    #         fill = "gray",
    #       ), alpha = 0.7) +
    #       ggplot2::geom_point(
    #         data = data,
    #         ggplot2::aes(y = SL, x = Age, colour = "black"), size = 0.3
    #       ) +
    #       ggplot2::geom_line(
    #         data = output_dataframes,
    #         ggplot2::aes(x = Age, y = pred, colour = "mean")
    #       ) +
    #       ggplot2::geom_ribbon(
    #         data = output_dataframes,
    #         ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age, fill = "CI"), alpha = 0.2
    #       ) +
    #       ggplot2::labs(x = xlab, y = ylab, title = title) +
    #       ggplot2::theme_bw() +
    #       ggplot2::theme(
    #         plot.title = ggplot2::element_text(size = 15),
    #         axis.title = ggplot2::element_text(size = 12, face = "bold"),
    #         axis.text = ggplot2::element_text(size = 12),
    #         legend.text = ggplot2::element_text(size = 10)
    #       ) +
    #       ggplot2::theme(
    #         strip.text.x = ggplot2::element_text(size = 10),
    #         strip.background = ggplot2::element_rect(fill = c("white"))
    #       ) +
    #       ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
    #       ggplot2::labs(colour = "") +
    #       ggplot2::scale_fill_manual("",
    #         values = c(
    #           "Uncertainty" = ggplot2::alpha("grey", 0.3),
    #           "CI" = ggplot2::alpha("purple3", 0.2)
    #         ),
    #         labels = c(
    #           CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
    #           expression(paste("1-sigma Error"))
    #         )
    #       ) +
    #       ggplot2::scale_colour_manual("",
    #         values = c("black" = "black", "mean" = "purple3"),
    #         labels = c("Data", "Posterior Fit")
    #       ) +
    #       ggplot2::guides(
    #         fill = ggplot2::guide_legend(override.aes = list(
    #           alpha = c(0.4, 0.2), # , 0.4),
    #           size = 1
    #         )),
    #         colour = ggplot2::guide_legend(override.aes = list(
    #           linetype = c(0, 1),
    #           shape = c(16, NA),
    #           size = 2
    #         ))
    #       ) +
    #       ggplot2::facet_wrap(~SiteName)
    #
    #     # Plotting Rate of Change for Total component----------
    #     plot_rate <-
    #       ggplot2::ggplot() +
    #       ggplot2::geom_line(
    #         data = output_dataframes,
    #         ggplot2::aes(x = Age, y = rate_pred, colour = "mean")
    #       ) +
    #       ggplot2::geom_ribbon(
    #         data = output_dataframes,
    #         ggplot2::aes(y = rate_pred, ymin = rate_lwr, ymax = rate_upr, x = Age, fill = "CI"), alpha = 0.2
    #       ) +
    #       ggplot2::labs(x = xlab, y = y_rate_lab, title = title) +
    #       ggplot2::theme_bw() +
    #       ggplot2::theme(
    #         plot.title = ggplot2::element_text(size = 15),
    #         axis.title = ggplot2::element_text(size = 12, face = "bold"),
    #         axis.text = ggplot2::element_text(size = 12),
    #         legend.text = ggplot2::element_text(size = 10)
    #       ) +
    #       ggplot2::theme(
    #         strip.text.x = ggplot2::element_text(size = 10),
    #         strip.background = ggplot2::element_rect(fill = c("white"))
    #       ) +
    #       ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
    #       ggplot2::labs(colour = "") +
    #       ggplot2::scale_fill_manual("",
    #         values = c(
    #           "CI" = ggplot2::alpha("purple3", 0.2)
    #         ),
    #         labels = c(
    #           CI = paste0(unique(output_dataframes$CI), " Credible Interval")
    #         )
    #       ) +
    #       ggplot2::scale_colour_manual("",
    #         values = c("mean" = "purple3"),
    #         labels = c("Posterior Fit")
    #       ) +
    #       ggplot2::geom_hline(yintercept = 0) +
    #       ggplot2::guides(
    #         fill = ggplot2::guide_legend(override.aes = list(
    #           alpha = c(0.4), # , 0.4),
    #           size = 1
    #         )),
    #         colour = ggplot2::guide_legend(override.aes = list(
    #           linetype = c(1),
    #           shape = c(NA),
    #           size = 2
    #         ))
    #       ) +
    #       ggplot2::facet_wrap(~SiteName)
    #   }
    # } else {
    #   if (plot_tide_gauges == FALSE) {
    #     output_dataframes <- jags_output$output_dataframes %>%
    #       dplyr::mutate(data_type_id = as.factor(data_type_id)) %>%
    #       dplyr::filter(data_type_id == "ProxyRecord")
    #     data <- jags_output$data
    #     n_sites <- length(data$SiteName %>% unique())
    #     n_proxy <- data %>%
    #       dplyr::filter(data_type_id == "ProxyRecord") %>%
    #       dplyr::select(SiteName, data_type_id) %>%
    #       unique() %>%
    #       nrow()
    #     data <- jags_output$data %>%
    #       dplyr::mutate(data_type_id = as.factor(data_type_id)) %>%
    #       dplyr::filter(data_type_id == "ProxyRecord")
    #
    #     # Plot result with informed caption
    #     if (plot_caption == TRUE) {
    #       plot_result <- create_model_fit_plot(
    #         output_dataframes = output_dataframes,
    #         data = data,
    #         xlab = xlab,
    #         ylab = ylab,
    #         title = title,
    #         model_caption = paste0(
    #           "Model type: Errors in Variables Integrated Gaussian Process \n No. proxy sites:", n_proxy,
    #           "\n No. tide gauge sites:", n_sites - n_proxy
    #         )
    #       )
    #       # Plotting Rate of Change for Total component----------
    #       plot_rate <-
    #         ggplot2::ggplot() +
    #         ggplot2::geom_line(
    #           data = output_dataframes,
    #           ggplot2::aes(x = Age, y = rate_pred, colour = "mean")
    #         ) +
    #         ggplot2::geom_ribbon(
    #           data = output_dataframes,
    #           ggplot2::aes(y = rate_pred, ymin = rate_lwr, ymax = rate_upr, x = Age, fill = "CI"), alpha = 0.2
    #         ) +
    #         ggplot2::labs(x = xlab, y = y_rate_lab, title = title) +
    #         ggplot2::theme_bw() +
    #         ggplot2::theme(
    #           plot.title = ggplot2::element_text(size = 15),
    #           axis.title = ggplot2::element_text(size = 12, face = "bold"),
    #           axis.text = ggplot2::element_text(size = 12),
    #           legend.text = ggplot2::element_text(size = 10)
    #         ) +
    #         ggplot2::theme(
    #           strip.text.x = ggplot2::element_text(size = 10),
    #           strip.background = ggplot2::element_rect(fill = c("white"))
    #         ) +
    #         ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
    #         ggplot2::labs(colour = "") +
    #         ggplot2::scale_fill_manual("",
    #           values = c(
    #             "CI" = ggplot2::alpha("purple3", 0.2)
    #           ),
    #           labels = c(
    #             CI = paste0(unique(output_dataframes$CI), " Credible Interval")
    #           )
    #         ) +
    #         ggplot2::scale_colour_manual("",
    #           values = c("mean" = "purple3"),
    #           labels = c("Posterior Fit")
    #         ) +
    #         ggplot2::geom_hline(yintercept = 0) +
    #         ggplot2::guides(
    #           fill = ggplot2::guide_legend(override.aes = list(
    #             alpha = c(0.4), # , 0.4),
    #             size = 1
    #           )),
    #           colour = ggplot2::guide_legend(override.aes = list(
    #             linetype = c(1),
    #             shape = c(NA),
    #             size = 2
    #           ))
    #         ) +
    #         ggplot2::facet_wrap(~SiteName) +
    #         ggplot2::labs(caption = paste0(
    #           "Model type: Errors in Variables Integrated Gaussian Process \n No. proxy sites:", n_proxy,
    #           "\n No. tide gauge sites:", n_sites - n_proxy
    #         ))
    #     } else {
    #       # Plot model fit
    #       plot_result <- create_model_fit_plot(
    #         output_dataframes = output_dataframes,
    #         data = data,
    #         model_caption = NULL,
    #         xlab = xlab,
    #         ylab = ylab,
    #         title = title
    #       )
    #       # Plot rate
    #       plot_rate <- create_rate_of_change_plot(
    #         output_dataframes = output_dataframes,
    #         data = data,
    #         model_caption = NULL,
    #         xlab = xlab,
    #         ylab = ylab,
    #         title = title
    #       )
    #       # Plotting Rate of Change for Total component----------
    #       plot_rate <-
    #         ggplot2::ggplot() +
    #         ggplot2::geom_line(
    #           data = output_dataframes,
    #           ggplot2::aes(x = Age, y = rate_pred, colour = "mean")
    #         ) +
    #         ggplot2::geom_ribbon(
    #           data = output_dataframes,
    #           ggplot2::aes(y = rate_pred, ymin = rate_lwr, ymax = rate_upr, x = Age, fill = "CI"), alpha = 0.2
    #         ) +
    #         ggplot2::labs(x = xlab, y = y_rate_lab, title = title) +
    #         ggplot2::theme_bw() +
    #         ggplot2::theme(
    #           plot.title = ggplot2::element_text(size = 15),
    #           axis.title = ggplot2::element_text(size = 12, face = "bold"),
    #           axis.text = ggplot2::element_text(size = 12),
    #           legend.text = ggplot2::element_text(size = 10)
    #         ) +
    #         ggplot2::theme(
    #           strip.text.x = ggplot2::element_text(size = 10),
    #           strip.background = ggplot2::element_rect(fill = c("white"))
    #         ) +
    #         ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
    #         ggplot2::labs(colour = "") +
    #         ggplot2::scale_fill_manual("",
    #           values = c(
    #             "CI" = ggplot2::alpha("purple3", 0.2)
    #           ),
    #           labels = c(
    #             CI = paste0(unique(output_dataframes$CI), " Credible Interval")
    #           )
    #         ) +
    #         ggplot2::scale_colour_manual("",
    #           values = c("mean" = "purple3"),
    #           labels = c("Posterior Fit")
    #         ) +
    #         ggplot2::geom_hline(yintercept = 0) +
    #         ggplot2::guides(
    #           fill = ggplot2::guide_legend(override.aes = list(
    #             alpha = c(0.4), # , 0.4),
    #             size = 1
    #           )),
    #           colour = ggplot2::guide_legend(override.aes = list(
    #             linetype = c(1),
    #             shape = c(NA),
    #             size = 2
    #           ))
    #         ) +
    #         ggplot2::facet_wrap(~SiteName)
    #     }
    #   } else {
    #     output_dataframes <- jags_output$output_dataframes
    #     data <- jags_output$data
    #     data <- jags_output$data
    #     n_sites <- length(data$SiteName %>% unique())
    #     n_proxy <- data %>%
    #       dplyr::filter(data_type_id == "ProxyRecord") %>%
    #       dplyr::select(SiteName, data_type_id) %>%
    #       unique() %>%
    #       nrow()
    #
    #     # Plot result with informed caption
    #     if (plot_caption == TRUE) {
    #       plot_result <- create_model_fit_plot(
    #         output_dataframes = output_dataframes,
    #         data = data,
    #         xlab = xlab,
    #         ylab = ylab,
    #         title = title,
    #         model_caption = paste0(
    #           "Model type: Errors in Variables Integrated Gaussian Process \n No. proxy sites:", n_proxy,
    #           "\n No. tide gauge sites:", n_sites - n_proxy
    #         )
    #       )
    #       # Plotting Rate of Change for Total component----------
    #       plot_rate <-
    #         ggplot2::ggplot() +
    #         ggplot2::geom_line(
    #           data = output_dataframes,
    #           ggplot2::aes(x = Age, y = rate_pred, colour = "mean")
    #         ) +
    #         ggplot2::geom_ribbon(
    #           data = output_dataframes,
    #           ggplot2::aes(y = rate_pred, ymin = rate_lwr, ymax = rate_upr, x = Age, fill = "CI"), alpha = 0.2
    #         ) +
    #         ggplot2::labs(x = xlab, y = y_rate_lab, title = title) +
    #         ggplot2::theme_bw() +
    #         ggplot2::theme(
    #           plot.title = ggplot2::element_text(size = 15),
    #           axis.title = ggplot2::element_text(size = 12, face = "bold"),
    #           axis.text = ggplot2::element_text(size = 12),
    #           legend.text = ggplot2::element_text(size = 10)
    #         ) +
    #         ggplot2::theme(
    #           strip.text.x = ggplot2::element_text(size = 10),
    #           strip.background = ggplot2::element_rect(fill = c("white"))
    #         ) +
    #         ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
    #         ggplot2::labs(colour = "") +
    #         ggplot2::scale_fill_manual("",
    #           values = c(
    #             "CI" = ggplot2::alpha("purple3", 0.2)
    #           ),
    #           labels = c(
    #             CI = paste0(unique(output_dataframes$CI), " Credible Interval")
    #           )
    #         ) +
    #         ggplot2::scale_colour_manual("",
    #           values = c("mean" = "purple3"),
    #           labels = c("Posterior Fit")
    #         ) +
    #         ggplot2::geom_hline(yintercept = 0) +
    #         ggplot2::guides(
    #           fill = ggplot2::guide_legend(override.aes = list(
    #             alpha = c(0.4), # , 0.4),
    #             size = 1
    #           )),
    #           colour = ggplot2::guide_legend(override.aes = list(
    #             linetype = c(1),
    #             shape = c(NA),
    #             size = 2
    #           ))
    #         ) +
    #         ggplot2::facet_wrap(~SiteName) +
    #         ggplot2::labs(caption = paste0(
    #           "Model type: Errors in Variables Integrated Gaussian Process \n No. proxy sites:", n_proxy,
    #           "\n No. tide gauge sites:", n_sites - n_proxy
    #         ))
    #     } else {
    #       plot_result <- create_model_fit_plot(
    #         output_dataframes = output_dataframes,
    #         data = data,
    #         xlab = xlab,
    #         ylab = ylab,
    #         title = title,
    #         model_caption = NULL
    #       )
    #       # Plotting Rate of Change for Total component----------
    #       plot_rate <-
    #         ggplot2::ggplot() +
    #         ggplot2::geom_line(
    #           data = output_dataframes,
    #           ggplot2::aes(x = Age, y = rate_pred, colour = "mean")
    #         ) +
    #         ggplot2::geom_ribbon(
    #           data = output_dataframes,
    #           ggplot2::aes(y = rate_pred, ymin = rate_lwr, ymax = rate_upr, x = Age, fill = "CI"), alpha = 0.2
    #         ) +
    #         ggplot2::labs(x = xlab, y = y_rate_lab, title = title) +
    #         ggplot2::theme_bw() +
    #         ggplot2::theme(
    #           plot.title = ggplot2::element_text(size = 15),
    #           axis.title = ggplot2::element_text(size = 12, face = "bold"),
    #           axis.text = ggplot2::element_text(size = 12),
    #           legend.text = ggplot2::element_text(size = 10)
    #         ) +
    #         ggplot2::theme(
    #           strip.text.x = ggplot2::element_text(size = 10),
    #           strip.background = ggplot2::element_rect(fill = c("white"))
    #         ) +
    #         ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
    #         ggplot2::labs(colour = "") +
    #         ggplot2::scale_fill_manual("",
    #           values = c(
    #             "CI" = ggplot2::alpha("purple3", 0.2)
    #           ),
    #           labels = c(
    #             CI = paste0(unique(output_dataframes$CI), " Credible Interval")
    #           )
    #         ) +
    #         ggplot2::scale_colour_manual("",
    #           values = c("mean" = "purple3"),
    #           labels = c("Posterior Fit")
    #         ) +
    #         ggplot2::geom_hline(yintercept = 0) +
    #         ggplot2::guides(
    #           fill = ggplot2::guide_legend(override.aes = list(
    #             alpha = c(0.4), # , 0.4),
    #             size = 1
    #           )),
    #           colour = ggplot2::guide_legend(override.aes = list(
    #             linetype = c(1),
    #             shape = c(NA),
    #             size = 2
    #           ))
    #         ) +
    #         ggplot2::facet_wrap(~SiteName)
    #     }
    #   }
    # }
    # # cat("Plotted EIV-IGP model & rate \n")
    output_plots <- list(plot_result = plot_result, plot_rate = plot_rate)
  }

  # NI spline time
  if (inherits(jags_output, "ni_spline_t") == TRUE) {
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data
    n_sites <- length(data$SiteName %>% unique())
    n_proxy <- data %>%
      dplyr::filter(data_type_id == "ProxyRecord") %>%
      dplyr::select(SiteName, data_type_id) %>%
      unique() %>%
      nrow()
    n_tide_gauges <- n_sites - n_proxy
    # Plotting proxy only
    if (plot_proxy_records == TRUE & plot_tide_gauges == FALSE) {
      output_dataframes <- output_dataframes %>%
        dplyr::filter(data_type_id == "ProxyRecord")
      data <- data %>%
        dplyr::filter(data_type_id == "ProxyRecord")
    }
    # Plotting tide gauge only
    if (plot_proxy_records == FALSE & plot_tide_gauges == TRUE) {
      output_dataframes <- output_dataframes %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      data <- data %>%
        dplyr::filter(data_type_id == "TideGaugeData")
    }

    # Using caption or not
    if (plot_caption == TRUE) {
      # Plot model fit
      plot_result <- create_model_fit_plot(
        output_dataframes = output_dataframes,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = paste0(
          "Model type: Noisy Input Spline in Time \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        )
      )

      # Plot rate
      plot_rate <- create_rate_of_change_plot(
        output_dataframes = output_dataframes,
        data = data,
        model_caption = paste0(
          "Model type: Noisy Input Spline in Time \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ),
        xlab = xlab,
        y_rate_lab = y_rate_lab,
        title = title
      )
    } else {
      # Plot model fit
      plot_result <- create_model_fit_plot(
        output_dataframes = output_dataframes,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = NULL
      )

      # Plot rate
      plot_rate <- create_rate_of_change_plot(
        output_dataframes = output_dataframes,
        data = data,
        model_caption = NULL,
        xlab = xlab,
        y_rate_lab = y_rate_lab,
        title = title
      )
    }
    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result +
        ggplot2::facet_wrap(~SiteName, scales = "free")
      plot_rate <- plot_rate +
        ggplot2::facet_wrap(~SiteName, scales = "free")
    }
    output_plots <- list(
      plot_result = plot_result,
      plot_rate = plot_rate
    )
  }

  # NI spline space time-------------
  if (inherits(jags_output, "ni_spline_st") == TRUE) {
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data
    n_sites <- length(data$SiteName %>% unique())
    n_proxy <- data %>%
      dplyr::filter(data_type_id == "ProxyRecord") %>%
      dplyr::select(SiteName, data_type_id) %>%
      unique() %>%
      nrow()
    n_tide_gauges <- n_sites - n_proxy
    # Plotting proxy only
    if (plot_proxy_records == TRUE & plot_tide_gauges == FALSE) {
      output_dataframes <- output_dataframes %>%
        dplyr::filter(data_type_id == "ProxyRecord")
      data <- data %>%
        dplyr::filter(data_type_id == "ProxyRecord")
    }
    # Plotting tide gauge only
    if (plot_proxy_records == FALSE & plot_tide_gauges == TRUE) {
      output_dataframes <- output_dataframes %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      data <- data %>%
        dplyr::filter(data_type_id == "TideGaugeData")
    }

    # Using caption or not
    if (plot_caption == TRUE) {
      # Plot model fit
      plot_result <- create_model_fit_plot(
        output_dataframes = output_dataframes,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = paste0(
          "Model type: Noisy Input Spline in Space Time \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        )
      )

      # Plot rate
      plot_rate <- create_rate_of_change_plot(
        output_dataframes = output_dataframes,
        data = data,
        model_caption = paste0(
          "Model type: Noisy Input Spline in Space Time \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ),
        xlab = xlab,
        y_rate_lab = y_rate_lab,
        title = title
      )
    } else {
      # Plot model fit
      plot_result <- create_model_fit_plot(
        output_dataframes = output_dataframes,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = NULL
      )

      # Plot rate
      plot_rate <- create_rate_of_change_plot(
        output_dataframes = output_dataframes,
        data = data,
        model_caption = NULL,
        xlab = xlab,
        y_rate_lab = y_rate_lab,
        title = title
      )
    }
    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result +
        ggplot2::facet_wrap(~SiteName, scales = "free")
      plot_rate <- plot_rate +
        ggplot2::facet_wrap(~SiteName, scales = "free")
    }
    # # cat("NI spline in space time plotted and rate of change. \n")
    output_plots <- list(
      plot_result = plot_result,
      plot_rate = plot_rate
    )
  }

  # NIGAM decomposition----------
  if (inherits(jags_output, "ni_gam_decomp") == TRUE) {
    output_dataframes <- jags_output$output_dataframes
    data <- jags_output$data
    n_sites <- length(data$SiteName %>% unique())
    n_proxy <- data %>%
      dplyr::filter(data_type_id == "ProxyRecord") %>%
      dplyr::select(SiteName, data_type_id) %>%
      unique() %>%
      nrow()
    n_tide_gauges <- n_sites - n_proxy
    # All the dataframes for each component
    total_model_fit_df <- output_dataframes$total_model_fit_df %>%
      dplyr::mutate(data_type_id = as.factor(data_type_id))
    total_model_rate_df <- output_dataframes$total_model_rate_df %>%
      dplyr::mutate(data_type_id = as.factor(data_type_id))
    regional_component_df <- output_dataframes$regional_component_df %>%
      dplyr::mutate(data_type_id = as.factor(data_type_id))
    regional_rate_component_df <- output_dataframes$regional_rate_component_df %>%
      dplyr::mutate(data_type_id = as.factor(data_type_id))
    lin_loc_component_df <- output_dataframes$lin_loc_component_df %>%
      dplyr::mutate(data_type_id = as.factor(data_type_id))
    non_lin_loc_component_df <- output_dataframes$non_lin_loc_component_df %>%
      dplyr::mutate(data_type_id = as.factor(data_type_id))
    non_lin_loc_rate_component_df <- output_dataframes$non_lin_loc_rate_component_df %>%
      dplyr::mutate(data_type_id = as.factor(data_type_id))


    # Plotting proxy only
    if (plot_proxy_records == TRUE & plot_tide_gauges == FALSE) {
      data <- data %>%
        dplyr::filter(data_type_id == "ProxyRecord")
      total_model_fit_df <- total_model_fit_df %>%
        dplyr::filter(data_type_id == "ProxyRecord")
      total_model_rate_df <- total_model_rate_df %>%
        dplyr::filter(data_type_id == "ProxyRecord")

      regional_component_df <- regional_component_df # %>%
      # dplyr::filter(data_type_id == "ProxyRecord")
      regional_rate_component_df <- regional_rate_component_df # %>%
      # dplyr::filter(data_type_id == "ProxyRecord")

      lin_loc_component_df <- lin_loc_component_df %>%
        dplyr::filter(data_type_id == "ProxyRecord")
      non_lin_loc_component_df <- non_lin_loc_component_df %>%
        dplyr::filter(data_type_id == "ProxyRecord")
      non_lin_loc_rate_component_df <- non_lin_loc_rate_component_df %>%
        dplyr::filter(data_type_id == "ProxyRecord")
    }
    # Plotting tide gauge only
    if (plot_proxy_records == FALSE & plot_tide_gauges == TRUE) {
      data <- data %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      total_model_fit_df <- total_model_fit_df %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      total_model_rate_df <- total_model_rate_df %>%
        dplyr::filter(data_type_id == "TideGaugeData")

      regional_component_df <- regional_component_df # %>%
      # dplyr::filter(data_type_id == "TideGaugeData")
      regional_rate_component_df <- regional_rate_component_df # %>%
      # dplyr::filter(data_type_id == "TideGaugeData")

      lin_loc_component_df <- lin_loc_component_df %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      non_lin_loc_component_df <- non_lin_loc_component_df %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      non_lin_loc_rate_component_df <- non_lin_loc_rate_component_df %>%
        dplyr::filter(data_type_id == "TideGaugeData")
    }


    # Using caption or not
    if (plot_caption == TRUE) {
      # Plot total model fit
      plot_result <- create_model_fit_plot(
        output_dataframes = total_model_fit_df,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        )
      )

      # Plot total model rate of change plot
      plot_rate <- create_rate_of_change_plot(
        output_dataframes = total_model_rate_df,
        data = data,
        model_caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ),
        xlab = xlab,
        y_rate_lab = y_rate_lab,
        title = title
      )
      # Regional Component Plot---------------------------
      regional_plot <-
        ggplot2::ggplot() +
        ggplot2::geom_line(
          data = regional_component_df,
          ggplot2::aes(x = Age, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = regional_component_df,
          ggplot2::aes(ymin = lwr, ymax = upr, x = Age, fill = "CI"), alpha = 0.2
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 15),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          axis.text = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 12)
        ) +
        ggplot2::scale_fill_manual("",
          values = c(
            "CI" = ggplot2::alpha("#3b47ad", 0.2)
          ),
          labels = c(
            CI = paste0(unique(regional_component_df$CI), " Credible Interval")
          )
        ) +
        ggplot2::scale_colour_manual("",
          values = c(
            "mean" = "#3b47ad"
          ),
          labels = c("Posterior Fit")
        ) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::ylab("Sea Level (m)") +
        ggplot2::xlab("Year (CE)") +
        ggplot2::labs(caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ))

      # Derivative of Predicted Regional Component Plot---------------------------
      regional_rate_plot <-
        ggplot2::ggplot() +
        ggplot2::geom_line(
          data = regional_rate_component_df,
          ggplot2::aes(x = Age, y = rate_pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = regional_rate_component_df,
          ggplot2::aes(ymin = rate_lwr, ymax = rate_upr, x = Age, fill = "CI"), alpha = 0.2
        ) +
        ggplot2::theme_bw() +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::scale_fill_manual("",
          values = c(
            "CI" = ggplot2::alpha("#3b47ad", 0.2)
          ),
          labels = c(
            CI = paste0(unique(regional_rate_component_df$CI), " Credible Interval")
          )
        ) +
        ggplot2::scale_colour_manual("",
          values = c(
            "mean" = "#3b47ad"
          ),
          labels = c("Posterior Fit")
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 15),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          axis.text = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 12),
          strip.text.x = ggplot2::element_text(size = 10),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::ylab("Rate of Change (mm/yr)") +
        ggplot2::xlab("Year (CE)") +
        ggplot2::labs(caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        )) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::labs(colour = "")

      # Linear Local + Site specific vertical offset ---------------------
      lin_loc_plot <-
        ggplot2::ggplot() +
        ggplot2::geom_line(
          data = lin_loc_component_df,
          ggplot2::aes(x = Age, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = lin_loc_component_df,
          ggplot2::aes(
            ymin = lwr, ymax = upr, x = Age,
            fill = "CI"
          ), alpha = 0.3
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 22),
          axis.title = ggplot2::element_text(size = 14, face = "bold"),
          axis.text = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 12)
        ) +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 12),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::scale_colour_manual("",
          values = c(
            "mean" = "#5bac06"
          ),
          labels = c("Posterior Fit")
        ) +
        ggplot2::scale_fill_manual("",
          values = c(
            "CI" = ggplot2::alpha("#5bac06", 0.2)
          ),
          labels = c(
            CI = paste0(unique(lin_loc_component_df$CI), " Credible Interval")
          )
        ) +
        ggplot2::ylab("Sea Level (m)") +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::xlab("Year (CE)") +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::labs(colour = "") +
        ggplot2::labs(caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ))

      # Plot Non linear local plot
      non_lin_loc_plot <- create_model_fit_plot(
        output_dataframes = non_lin_loc_component_df,
        data = data,
        xlab = xlab,
        ylab = "Sea Level (m)",
        title = title,
        model_caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ),
        plot_colour = "#ad4c14"
      )

      # Plot rate for non linear local
      non_lin_loc_rate_plot <- create_rate_of_change_plot(
        output_dataframes = non_lin_loc_rate_component_df,
        data = data,
        model_caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ),
        xlab = xlab,
        y_rate_lab = y_rate_lab,
        title = title,
        plot_colour = "#ad4c14"
      )

      # Separate Components on one plot with CI --------
      all_components_CI_plot <- ggplot2::ggplot() +
        # Linear Local Component + site specific vertical offset
        ggplot2::geom_line(
          data = lin_loc_component_df,
          ggplot2::aes(x = Age, y = pred, colour = "Site Specific vertical offset + \n Linear Local Component") # ID)
        ) +
        ggplot2::geom_ribbon(
          data = lin_loc_component_df,
          ggplot2::aes(ymin = lwr, ymax = upr, x = Age, fill = "Site Specific vertical offset + \n Linear Local Component"), # ID),
          alpha = 0.3
        ) +

        # Non linear Local
        ggplot2::geom_line(
          data = non_lin_loc_component_df,
          ggplot2::aes(x = Age, y = pred, colour = "Non Linear Local Component") # ID)
        ) +
        ggplot2::geom_ribbon(
          data = non_lin_loc_component_df,
          ggplot2::aes(ymin = lwr, ymax = upr, x = Age, fill = "Non Linear Local Component"), # ID),
          alpha = 0.3
        ) +
        # Regional Component
        ggplot2::geom_line(
          data = regional_component_df,
          ggplot2::aes(x = Age, y = pred, colour = "Regional Component") # ID)
        ) +
        ggplot2::geom_ribbon(
          data = regional_component_df,
          ggplot2::aes(ymin = lwr, ymax = upr, x = Age, fill = "Regional Component"), # ID),
          alpha = 0.3
        ) +
        # Total Model
        ggplot2::geom_line(
          data = total_model_fit_df,
          ggplot2::aes(x = Age, y = pred, colour = "Total Posterior Model") # ID)
        ) +
        ggplot2::geom_ribbon(
          data = total_model_fit_df,
          ggplot2::aes(ymin = lwr, ymax = upr, x = Age, fill = "Total Posterior Model"), # ID),
          alpha = 0.3
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 9),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::scale_fill_manual(
          name = "",
          values = c(
            "Site Specific vertical offset + \n Linear Local Component" = "#5bac06",
            "Non Linear Local Component" = "#ad4c14",
            "Regional Component" = "#3b47ad",
            "Total Posterior Model" = "purple3"
          ),
          guide = ggplot2::guide_legend(override.aes = list(alpha = 0.1))
        ) +
        ggplot2::scale_colour_manual(
          name = "",
          values = c(
            "Site Specific vertical offset + \n Linear Local Component" = "#5bac06",
            "Non Linear Local Component" = "#ad4c14",
            "Regional Component" = "#3b47ad",
            "Total Posterior Model" = "purple3"
          ),
        ) +
        ggplot2::ylab("Sea Level (m)") +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::xlab("Year (CE)") +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::labs(caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ))
    } else {
      # Plot model fit
      plot_result <- create_model_fit_plot(
        output_dataframes = total_model_fit_df,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title,
        model_caption = NULL
      )

      # Plot rate
      plot_rate <- create_rate_of_change_plot(
        output_dataframes = total_rate_df,
        data = data,
        model_caption = NULL,
        xlab = xlab,
        y_rate_lab = y_rate_lab,
        title = title
      )
      # Regional Component Plot---------------------------
      regional_plot <-
        ggplot2::ggplot() +
        ggplot2::geom_line(
          data = regional_component_df,
          ggplot2::aes(x = Age, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = regional_component_df,
          ggplot2::aes(ymin = lwr, ymax = upr, x = Age, fill = "CI"), alpha = 0.2
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 15),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          axis.text = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 12)
        ) +
        ggplot2::scale_fill_manual("",
          values = c(
            "CI" = ggplot2::alpha("#3b47ad", 0.2)
          ),
          labels = c(
            CI = paste0(unique(regional_component_df$CI), " Credible Interval")
          )
        ) +
        ggplot2::scale_colour_manual("",
          values = c(
            "mean" = "#3b47ad"
          ),
          labels = c("Posterior Fit")
        ) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::ylab("Sea Level (m)") +
        ggplot2::xlab("Year (CE)") +
        ggplot2::labs(caption = "")

      # Derivative of Predicted Regional Component Plot---------------------------
      regional_rate_plot <-
        ggplot2::ggplot() +
        ggplot2::geom_line(
          data = regional_rate_component_df,
          ggplot2::aes(x = Age, y = rate_pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = regional_rate_component_df,
          ggplot2::aes(ymin = rate_lwr, ymax = rate_upr, x = Age, fill = "CI"), alpha = 0.2
        ) +
        ggplot2::theme_bw() +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::scale_fill_manual("",
          values = c(
            "CI" = ggplot2::alpha("#3b47ad", 0.2)
          ),
          labels = c(
            CI = paste0(unique(regional_rate_component_df$CI), " Credible Interval")
          )
        ) +
        ggplot2::scale_colour_manual("",
          values = c(
            "mean" = "#3b47ad"
          ),
          labels = c("Posterior Fit")
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 15),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          axis.text = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 12),
          strip.text.x = ggplot2::element_text(size = 10),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::ylab("Rate of Change (mm/yr)") +
        ggplot2::xlab("Year (CE)") +
        ggplot2::labs(caption = "") +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::labs(colour = "")

      # Linear Local + Site specific vertical offset ---------------------
      lin_loc_plot <-
        ggplot2::ggplot() +
        ggplot2::geom_line(
          data = lin_loc_component_df,
          ggplot2::aes(x = Age, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = lin_loc_component_df,
          ggplot2::aes(
            ymin = lwr, ymax = upr, x = Age,
            fill = "CI"
          ), alpha = 0.3
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 22),
          axis.title = ggplot2::element_text(size = 14, face = "bold"),
          axis.text = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 12)
        ) +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 12),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::scale_colour_manual("",
          values = c(
            "mean" = "#5bac06"
          ),
          labels = c("Posterior Fit")
        ) +
        ggplot2::scale_fill_manual("",
          values = c(
            "CI" = ggplot2::alpha("#5bac06", 0.2)
          ),
          labels = c(
            CI = paste0(unique(lin_loc_component_df$CI), " Credible Interval")
          )
        ) +
        ggplot2::ylab("Sea Level (m)") +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::xlab("Year (CE)") +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::labs(colour = "") +
        ggplot2::labs(caption = "")

      # Plot Non linear local plot
      non_lin_loc_plot <- create_model_fit_plot(
        output_dataframes = non_lin_loc_component_df,
        data = data,
        xlab = xlab,
        ylab = "Sea Level (m)",
        title = title,
        model_caption = NULL,
        plot_colour = "#ad4c14"
      )

      # Plot rate for non linear local
      non_lin_loc_rate_plot <- create_rate_of_change_plot(
        output_dataframes = non_lin_loc_rate_component_df,
        data = data,
        model_caption = NULL,
        xlab = xlab,
        y_rate_lab = y_rate_lab,
        title = title,
        plot_colour = "#ad4c14"
      )
      # Separate Components on one plot with CI --------
      all_components_CI_plot <- ggplot2::ggplot() +
        # Linear Local Component + site specific vertical offset
        ggplot2::geom_line(
          data = lin_loc_component_df,
          ggplot2::aes(x = Age, y = pred, colour = "Site Specific vertical offset + \n Linear Local Component") # ID)
        ) +
        ggplot2::geom_ribbon(
          data = lin_loc_component_df,
          ggplot2::aes(ymin = lwr, ymax = upr, x = Age, fill = "Site Specific vertical offset + \n Linear Local Component"), # ID),
          alpha = 0.3
        ) +

        # Non linear Local
        ggplot2::geom_line(
          data = non_lin_loc_component_df,
          ggplot2::aes(x = Age, y = pred, colour = "Non Linear Local Component") # ID)
        ) +
        ggplot2::geom_ribbon(
          data = non_lin_loc_component_df,
          ggplot2::aes(ymin = lwr, ymax = upr, x = Age, fill = "Non Linear Local Component"), # ID),
          alpha = 0.3
        ) +
        # Regional Component
        ggplot2::geom_line(
          data = regional_component_df,
          ggplot2::aes(x = Age, y = pred, colour = "Regional Component") # ID)
        ) +
        ggplot2::geom_ribbon(
          data = regional_component_df,
          ggplot2::aes(ymin = lwr, ymax = upr, x = Age, fill = "Regional Component"), # ID),
          alpha = 0.3
        ) +
        # Total Model
        ggplot2::geom_line(
          data = total_model_fit_df,
          ggplot2::aes(x = Age, y = pred, colour = "Total Posterior Model") # ID)
        ) +
        ggplot2::geom_ribbon(
          data = total_model_fit_df,
          ggplot2::aes(ymin = lwr, ymax = upr, x = Age, fill = "Total Posterior Model"), # ID),
          alpha = 0.3
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 9),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::scale_fill_manual(
          name = "",
          values = c(
            "Site Specific vertical offset + \n Linear Local Component" = "#5bac06",
            "Non Linear Local Component" = "#ad4c14",
            "Regional Component" = "#3b47ad",
            "Total Posterior Model" = "purple3"
          ),
          guide = ggplot2::guide_legend(override.aes = list(alpha = 0.1))
        ) +
        ggplot2::scale_colour_manual(
          name = "",
          values = c(
            "Site Specific vertical offset + \n Linear Local Component" = "#5bac06",
            "Non Linear Local Component" = "#ad4c14",
            "Regional Component" = "#3b47ad",
            "Total Posterior Model" = "purple3"
          ),
        ) +
        ggplot2::ylab("Sea Level (m)") +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::xlab("Year (CE)") +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::labs(caption = "")
    }
    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result +
        ggplot2::facet_wrap(~SiteName, scales = "free")
      plot_rate <- plot_rate +
        ggplot2::facet_wrap(~SiteName, scales = "free")
      regional_plot <- regional_plot
      regional_rate_plot <- regional_rate_plot
      lin_loc_plot <- lin_loc_plot +
        ggplot2::facet_wrap(~SiteName, scales = "free")
      non_lin_loc_plot <- non_lin_loc_plot +
        ggplot2::facet_wrap(~SiteName, scales = "free")
      non_lin_loc_rate_plot <- non_lin_loc_rate_plot +
        ggplot2::facet_wrap(~SiteName, scales = "free")
      all_components_CI_plot <- all_components_CI_plot +
        ggplot2::facet_wrap(~SiteName, scales = "free")
    }

    output_plots <- list(
      plot_result = plot_result,
      plot_rate = plot_rate,
      regional_plot = regional_plot,
      regional_rate_plot = regional_rate_plot,
      lin_loc_plot = lin_loc_plot,
      non_lin_loc_plot = non_lin_loc_plot,
      non_lin_loc_rate_plot = non_lin_loc_rate_plot,
      all_components_CI_plot = all_components_CI_plot
    )
  }
  if ("model_fit_plot" %in% plot_type) {
    # message("Print plot of the model fit\n")
    return(output_plots$plot_result)
  }
  if ("rate_plot" %in% plot_type) {
    # message("Print plot of the rate of change \n")
    return(output_plots$plot_rate)
  }
  if ("regional_plot" %in% plot_type) {
    # message("Print plot of the regional component \n")
    return(output_plots$regional_plot)
  }
  if ("regional_rate_plot" %in% plot_type) {
    # message("Print plot of the rate of change for the regional component \n")
    return(output_plots$regional_rate_plot)
  }
  if ("linear_local_plot" %in% plot_type) {
    # message("Print plot of the linear local component \n")
    return(output_plots$lin_loc_plot)
  }
  if ("non_linear_local_plot" %in% plot_type) {
    # message("Print plot of the non-linear local component \n")
    return(output_plots$non_lin_loc_plot)
  }
  if ("non_linear_local_rate_plot" %in% plot_type) {
    # message("Print plot of the rate of change of the non-linear local component \n")
    return(output_plots$non_lin_loc_rate_plot)
  }
  if ("nigam_component_plot" %in% plot_type) {
    # message("Print plot of all the components of the NI GAM decomposition \n")
    return(output_plots$all_components_CI_plot)
  }
}
