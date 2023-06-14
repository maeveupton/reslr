#' Plotting the results for each statistical model from the \code{reslr_mcmc} function.
#'
#' Depending on the model chosen in the \code{reslr_mcmc} function, the package produces a range of output plots.
#' Total posterior model fit plot with the raw data and measurement uncertainty are created for each statistical model.
#' The rate of change plots are created for the EIV IGP and the NI spline regression models.
#' For the NI GAM decomposition, each individual component of the model is plotted. Also, the regional and the non-linear local component, an associated rate plot is produced.
#' If tide gauges are used in the model, the user has the ability plot the output with or without this additional data source.
#'
#' @param x An object of class \code{reslr_output} and \code{model_type} created via \code{\link{reslr_mcmc}}
#' @param plot_proxy_records Plotting the proxy records on their own and this is the default
#' @param plot_tide_gauges Plotting the tide gauge data as well as proxy data
#' @param plot_type The user can select the type of output plot they require from the following: "rate_plot", "model_fit_plot", "regional_plot", "regional_rate_plot", "linear_local_plot", "non_linear_local_plot", "non_linear_local_rate_plot", "nigam_component_plot"
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
#' \donttest{
#' data <- NAACproxydata %>% dplyr::filter(Site == "Cedar Island")
#' x <- reslr_load(data = data)
#' jags_output <- reslr_mcmc(x, model_type = "eiv_slr_t")
#' plot(x = jags_output)}
plot.reslr_output <- function(x,
                              plot_proxy_records = TRUE,
                              plot_tide_gauges = FALSE,
                              title = "",
                              plot_type = c("model_fit_plot"),
                              plot_caption = TRUE,
                              xlab = "Year (CE)",
                              ylab = "Relative Sea Level (m)",
                              y_rate_lab = "Rate of change (mm per year)",
                              ...) {
  Age <- Age_BP <- RSL <- SL <- y_lwr_box <- y_upr_box <- Age_err <- ID <- RSL_err <- obs_index <- lwr <- upr <- lwr <- rate_pred <- rate_lwr <- rate_upr <- SiteName <- data_type_id <- pred <- NULL
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
    # Plot result
    plot_result <- create_model_fit_plot(
      output_dataframes = output_dataframes,
      data = data,
      xlab = xlab,
      ylab = ylab,
      title = title
    )

    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result + ggplot2::facet_wrap(~SiteName, scales = "free")
    }

    # Plotting when Age type is  BP
    if ("Age_type" %in% colnames(data)) {
      plot_result <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = data, ggplot2::aes(
          xmin = Age_BP - Age_err, xmax = Age_BP + Age_err,
          ymin = RSL - RSL_err, ymax = RSL + RSL_err,
          fill = "Uncertainty",
        ), alpha = 0.7) +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(y = RSL, x = Age_BP, colour = "black"), size = 0.3
        ) +
        ggplot2::geom_line(
          data = output_dataframes,
          ggplot2::aes(x = Age_BP, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = output_dataframes,
          ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age_BP, fill = "CI"), alpha = 0.3
        ) +
        ggplot2::scale_fill_manual("",
          values = c(
            "Uncertainty" = ggplot2::alpha("grey", 0.3),
            "CI" = ggplot2::alpha("purple3", 0.2)
          ),
          labels = c(
            CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
            Uncertainty = expression(paste("1-sigma Error"))
          )
        ) +
        ggplot2::scale_colour_manual("",
          values = c(
            "black" = "black",
            "mean" = "purple3"
          ),
          labels = c("Data", "Posterior Fit")
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(override.aes = list(
            alpha = c(0.3, 0.2),
            size = 1
          )),
          colour = ggplot2::guide_legend(override.aes = list(
            linetype = c(0, 1),
            shape = c(16, NA),
            size = 2
          ))
        ) +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::labs(x = "Year (BP)", y = ylab, title = title, colour = "") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::scale_x_reverse()
    } else {
      plot_result <- plot_result
    }

    # Informed caption
    if (plot_caption == TRUE) {
      plot_result <- plot_result + ggplot2::labs(caption = paste0(
        "Model type: Errors in Variables Simple Linear Regression \n No. proxy sites:", n_proxy,
        "\n No. tide gauge sites:", n_sites - n_proxy
      ))
    } else {
      plot_result <- plot_result
    }
    # Output plots
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
    # Plot result
    plot_result <- create_model_fit_plot(
      output_dataframes = output_dataframes,
      data = data,
      xlab = xlab,
      ylab = ylab,
      title = title
    )

    # Plotting tide gauge and proxy
    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result + ggplot2::facet_wrap(~SiteName, scales = "free")
    }

    # Plotting when Age type is BP
    if ("Age_type" %in% colnames(data)) {
      plot_result <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = data, ggplot2::aes(
          xmin = Age_BP - Age_err, xmax = Age_BP + Age_err,
          ymin = RSL - RSL_err, ymax = RSL + RSL_err,
          fill = "Uncertainty",
        ), alpha = 0.7) +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(y = RSL, x = Age_BP, colour = "black"), size = 0.3
        ) +
        ggplot2::geom_line(
          data = output_dataframes,
          ggplot2::aes(x = Age_BP, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = output_dataframes,
          ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age_BP, fill = "CI"), alpha = 0.3
        ) +
        ggplot2::scale_fill_manual("",
          values = c(
            "Uncertainty" = ggplot2::alpha("grey", 0.3),
            "CI" = ggplot2::alpha("purple3", 0.2)
          ),
          labels = c(
            CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
            Uncertainty = expression(paste("1-sigma Error"))
          )
        ) +
        ggplot2::scale_colour_manual("",
          values = c(
            "black" = "black",
            "mean" = "purple3"
          ),
          labels = c("Data", "Posterior Fit")
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(override.aes = list(
            alpha = c(0.3, 0.2),
            size = 1
          )),
          colour = ggplot2::guide_legend(override.aes = list(
            linetype = c(0, 1),
            shape = c(16, NA),
            size = 2
          ))
        ) +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::labs(x = "Year (BP)", y = ylab, title = title, colour = "") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::scale_x_reverse()
    } else {
      plot_result <- plot_result
    }

    # Informed caption
    if (plot_caption == TRUE) {
      plot_result <- plot_result + ggplot2::labs(caption = paste0(
        "Model type:  Errors in Variables 1 Change Point Model \n No. proxy sites:", n_proxy,
        "\n No. tide gauge sites:", n_sites - n_proxy
      ))
    } else {
      plot_result <- plot_result
    }
    # Output plots
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

    # Plotting result
    plot_result <- create_model_fit_plot(
      output_dataframes = output_dataframes,
      data = data,
      xlab = xlab,
      ylab = ylab,
      title = title
    )

    # Plotting tide gauge and proxy records together
    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result + ggplot2::facet_wrap(~SiteName, scales = "free")
    }
    # Plotting when Age type is BP
    if ("Age_type" %in% colnames(data)) {
      plot_result <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = data, ggplot2::aes(
          xmin = Age_BP - Age_err, xmax = Age_BP + Age_err,
          ymin = RSL - RSL_err, ymax = RSL + RSL_err,
          fill = "Uncertainty",
        ), alpha = 0.7) +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(y = RSL, x = Age_BP, colour = "black"), size = 0.3
        ) +
        ggplot2::geom_line(
          data = output_dataframes,
          ggplot2::aes(x = Age_BP, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = output_dataframes,
          ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age_BP, fill = "CI"), alpha = 0.3
        ) +
        ggplot2::scale_fill_manual("",
          values = c(
            "Uncertainty" = ggplot2::alpha("grey", 0.3),
            "CI" = ggplot2::alpha("purple3", 0.2)
          ),
          labels = c(
            CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
            Uncertainty = expression(paste("1-sigma Error"))
          )
        ) +
        ggplot2::scale_colour_manual("",
          values = c(
            "black" = "black",
            "mean" = "purple3"
          ),
          labels = c("Data", "Posterior Fit")
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(override.aes = list(
            alpha = c(0.3, 0.2),
            size = 1
          )),
          colour = ggplot2::guide_legend(override.aes = list(
            linetype = c(0, 1),
            shape = c(16, NA),
            size = 2
          ))
        ) +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::labs(x = "Year (BP)", y = ylab, title = title, colour = "") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::scale_x_reverse()
    } else {
      plot_result <- plot_result
    }

    # Informed caption
    if (plot_caption == TRUE) {
      plot_result <- plot_result + ggplot2::labs(caption = paste0(
        "Model type:  Errors in Variables 2 Change Point Model \n No. proxy sites:", n_proxy,
        "\n No. tide gauge sites:", n_sites - n_proxy
      ))
    } else {
      plot_result <- plot_result
    }
    # Output plot
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

    # Plotting result
    plot_result <- create_model_fit_plot(
      output_dataframes = output_dataframes,
      data = data,
      xlab = xlab,
      ylab = ylab,
      title = title
    )

    # Plotting tide gauge and proxy records together
    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result + ggplot2::facet_wrap(~SiteName, scales = "free")
    }
    # Plotting when Age type is BP
    if ("Age_type" %in% colnames(data)) {
      plot_result <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = data, ggplot2::aes(
          xmin = Age_BP - Age_err, xmax = Age_BP + Age_err,
          ymin = RSL - RSL_err, ymax = RSL + RSL_err,
          fill = "Uncertainty",
        ), alpha = 0.7) +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(y = RSL, x = Age_BP, colour = "black"), size = 0.3
        ) +
        ggplot2::geom_line(
          data = output_dataframes,
          ggplot2::aes(x = Age_BP, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = output_dataframes,
          ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age_BP, fill = "CI"), alpha = 0.3
        ) +
        ggplot2::scale_fill_manual("",
                                   values = c(
                                     "Uncertainty" = ggplot2::alpha("grey", 0.3),
                                     "CI" = ggplot2::alpha("purple3", 0.2)
                                   ),
                                   labels = c(
                                     CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
                                     Uncertainty = expression(paste("1-sigma Error"))
                                   )
        ) +
        ggplot2::scale_colour_manual("",
                                     values = c(
                                       "black" = "black",
                                       "mean" = "purple3"
                                     ),
                                     labels = c("Data", "Posterior Fit")
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(override.aes = list(
            alpha = c(0.3, 0.2),
            size = 1
          )),
          colour = ggplot2::guide_legend(override.aes = list(
            linetype = c(0, 1),
            shape = c(16, NA),
            size = 2
          ))
        ) +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::labs(x = "Year (BP)", y = ylab, title = title, colour = "") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::scale_x_reverse()
    } else {
      plot_result <- plot_result
    }

    # Informed caption
    if (plot_caption == TRUE) {
      plot_result <- plot_result + ggplot2::labs(caption = paste0(
        "Model type:  Errors in Variables 3 Change Point Model \n No. proxy sites:", n_proxy,
        "\n No. tide gauge sites:", n_sites - n_proxy
      ))
    } else {
      plot_result <- plot_result
    }
    # Output plot
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
    if (inherits(jags_output, "detrend_data") == TRUE) {
      detrend_data_un_box <- jags_output$detrend_data_un_box
      # Plotting proxy only
      if (plot_proxy_records == TRUE & plot_tide_gauges == FALSE) {
        output_dataframes <- output_dataframes %>%
          dplyr::filter(data_type_id == "ProxyRecord")
        data <- data %>%
          dplyr::filter(data_type_id == "ProxyRecord")
        detrend_data_un_box <- detrend_data_un_box %>%
          dplyr::filter(data_type_id == "ProxyRecord")
      }
      # Plotting tide gauge only
      if (plot_proxy_records == FALSE & plot_tide_gauges == TRUE) {
        output_dataframes <- output_dataframes %>%
          dplyr::filter(data_type_id == "TideGaugeData")
        data <- data %>%
          dplyr::filter(data_type_id == "TideGaugeData")
        detrend_data_un_box <- detrend_data_un_box %>%
          dplyr::filter(data_type_id == "TideGaugeData")
      }
      # Plot model fit
      plot_result <- ggplot2::ggplot() +
        ggplot2::geom_polygon(
          data = detrend_data_un_box,
          ggplot2::aes(x = Age * 1000, y = SL, group = obs_index,
                       fill = "Uncertainty"), alpha = 0.5
        ) +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(y = SL, x = Age, colour = "black"), size = 0.3
        ) +
        ggplot2::geom_line(
          data = output_dataframes,
          ggplot2::aes(x = Age, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = output_dataframes,
          ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age, fill = "CI"), alpha = 0.3
        ) +
        ggplot2::labs(
          x = xlab,
          y = "Sea Level (m)", # ylab,
          title = title, colour = ""
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 18, face = "bold"),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.text = ggplot2::element_text(size = 10)
        ) +
        ggplot2::scale_fill_manual("",
                                   values = c(
                                     "Uncertainty" = ggplot2::alpha("grey", 0.3),
                                     "CI" = ggplot2::alpha("purple3", 0.2)
                                   ),
                                   labels = c(
                                     CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
                                     expression(paste("1-sigma Error"))
                                   )
        ) +
        ggplot2::scale_colour_manual("",
                                     values = c(
                                       "black" = "black",
                                       "mean" = "purple3"
                                     ),
                                     labels = c("Data", "Posterior Fit")
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(override.aes = list(
            alpha = c(0.2, 0.4),
            size = 1
          )),
          colour = ggplot2::guide_legend(override.aes = list(
            linetype = c(0, 1),
            shape = c(16, NA),
            size = 2
          ))
        ) +
        ggplot2::facet_wrap(~SiteName)

      # Plot rate
      plot_rate <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = output_dataframes,
          ggplot2::aes(x = Age, y = rate_pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = output_dataframes,
          ggplot2::aes(
            y = rate_pred,
            ymin = rate_lwr, ymax = rate_upr, x = Age, fill = "CI"
          ), alpha = 0.3
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::scale_fill_manual("",
                                   values = c(
                                     "CI" = ggplot2::alpha("purple3", 0.2)
                                   ),
                                   labels = c(
                                     CI = paste0(unique(output_dataframes$CI), " Credible Interval")
                                   )
        ) +
        ggplot2::scale_colour_manual("",
                                     values = c(
                                       "mean" = "purple3"
                                     ),
                                     labels = c("Posterior Fit")
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(override.aes = list(
            alpha = c(0.3), # , 0.4),
            size = 1
          )),
          colour = ggplot2::guide_legend(override.aes = list(
            linetype = c(1),
            shape = c(NA),
            size = 2
          ))
        ) +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 18, face = "bold"),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.text = ggplot2::element_text(size = 10)
        ) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::labs(
          x = xlab,
          y = y_rate_lab,
          title = title,
          colour = ""
        )

      # Age type BP
      if ("Age_type" %in% colnames(data)) {
        # Plot model fit
        plot_result <- ggplot2::ggplot() +
          ggplot2::geom_polygon(
            data = detrend_data_un_box,
            ggplot2::aes(x = Age_BP * 1000, y = SL, group = obs_index,
                         fill = "Uncertainty"), alpha = 0.5
          ) +
          ggplot2::geom_point(
            data = data,
            ggplot2::aes(y = SL, x = Age_BP, colour = "black"), size = 0.3
          ) +
          ggplot2::geom_line(
            data = output_dataframes,
            ggplot2::aes(x = Age_BP, y = pred, colour = "mean")
          ) +
          ggplot2::geom_ribbon(
            data = output_dataframes,
            ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age_BP, fill = "CI"), alpha = 0.3
          ) +
          ggplot2::labs(
            x = "Year (BP)",
            y = "Sea Level (m)",
            title = title, colour = ""
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            strip.text.x = ggplot2::element_text(size = 7),
            strip.background = ggplot2::element_rect(fill = c("white"))
          ) +
          ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 18, face = "bold"),
            axis.title = ggplot2::element_text(size = 12, face = "bold"),
            legend.text = ggplot2::element_text(size = 10)
          ) +
          ggplot2::scale_fill_manual("",
                                     values = c(
                                       "Uncertainty" = ggplot2::alpha("grey", 0.3),
                                       "CI" = ggplot2::alpha("purple3", 0.2)
                                     ),
                                     labels = c(
                                       CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
                                       expression(paste("1-sigma Error"))
                                     )
          ) +
          ggplot2::scale_colour_manual("",
                                       values = c(
                                         "black" = "black",
                                         "mean" = "purple3"
                                       ),
                                       labels = c("Data", "Posterior Fit")
          ) +
          ggplot2::guides(
            fill = ggplot2::guide_legend(override.aes = list(
              alpha = c(0.2, 0.4),
              size = 1
            )),
            colour = ggplot2::guide_legend(override.aes = list(
              linetype = c(0, 1),
              shape = c(16, NA),
              size = 2
            ))
          ) +
          ggplot2::facet_wrap(~SiteName)

        # Plot rate
        plot_rate <- ggplot2::ggplot() +
          ggplot2::geom_line(
            data = output_dataframes,
            ggplot2::aes(x = Age_BP, y = rate_pred, colour = "mean")
          ) +
          ggplot2::geom_ribbon(
            data = output_dataframes,
            ggplot2::aes(
              y = rate_pred,
              ymin = rate_lwr, ymax = rate_upr, x = Age_BP, fill = "CI"
            ), alpha = 0.3
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            strip.text.x = ggplot2::element_text(size = 7),
            strip.background = ggplot2::element_rect(fill = c("white"))
          ) +
          ggplot2::scale_fill_manual("",
                                     values = c(
                                       "CI" = ggplot2::alpha("purple3", 0.2)
                                     ),
                                     labels = c(
                                       CI = paste0(unique(output_dataframes$CI), " Credible Interval")
                                     )
          ) +
          ggplot2::scale_colour_manual("",
                                       values = c(
                                         "mean" = "purple3"
                                       ),
                                       labels = c("Posterior Fit")
          ) +
          ggplot2::guides(
            fill = ggplot2::guide_legend(override.aes = list(
              alpha = c(0.3), # , 0.4),
              size = 1
            )),
            colour = ggplot2::guide_legend(override.aes = list(
              linetype = c(1),
              shape = c(NA),
              size = 2
            ))
          ) +
          ggplot2::facet_wrap(~SiteName) +
          ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
          ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 18, face = "bold"),
            axis.title = ggplot2::element_text(size = 12, face = "bold"),
            legend.text = ggplot2::element_text(size = 10)
          ) +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::labs(
            x = "Year (BP)",
            y = y_rate_lab,
            title = title,
            colour = ""
          )

      }

      # Using informed caption or not
      if (plot_caption == TRUE) {
        plot_result <- plot_result + ggplot2::labs(caption = paste0(
          "Model type: Errors in Variables Integrated Gaussian Process Model using detrended data \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ))
        plot_rate <- plot_rate + ggplot2::labs(caption = paste0(
          "Model type: Errors in Variables Integrated Gaussian Process Model using detrended data \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ))
      } else {
        plot_result <- plot_result
        plot_rate <- plot_rate
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
        # Plot model fit
        plot_result <- create_model_fit_plot(
          output_dataframes = output_dataframes,
          data = data,
          xlab = xlab,
          ylab = ylab,
          title = title
        )

        # Plot rate
        plot_rate <- create_rate_of_change_plot(
          output_dataframes = output_dataframes,
          data = data,
          xlab = xlab,
          y_rate_lab = y_rate_lab,
          title = title
        )
        # Age type BP
        if ("Age_type" %in% colnames(data)) {
          # Plot model fit
          plot_result <- ggplot2::ggplot() +
            ggplot2::geom_rect(data = data, ggplot2::aes(
              xmin = Age_BP - Age_err, xmax = Age_BP + Age_err,
              ymin = RSL - RSL_err, ymax = RSL + RSL_err,
              fill = "Uncertainty",
            ), alpha = 0.7) +
            ggplot2::geom_point(
              data = data,
              ggplot2::aes(y = RSL, x = Age_BP, colour = "black"), size = 0.3
            ) +
            ggplot2::geom_line(
              data = output_dataframes,
              ggplot2::aes(x = Age_BP, y = pred, colour = "mean")
            ) +
            ggplot2::geom_ribbon(
              data = output_dataframes,
              ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age_BP, fill = "CI"), alpha = 0.3
            ) +
            ggplot2::labs(
              x = "Year (BP)",
              y = "Relative Sea Level (m)",
              title = title, colour = ""
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
              strip.text.x = ggplot2::element_text(size = 7),
              strip.background = ggplot2::element_rect(fill = c("white"))
            ) +
            ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
            ggplot2::theme(
              plot.title = ggplot2::element_text(size = 18, face = "bold"),
              axis.title = ggplot2::element_text(size = 12, face = "bold"),
              legend.text = ggplot2::element_text(size = 10)
            ) +
            ggplot2::scale_fill_manual("",
                                       values = c(
                                         "Uncertainty" = ggplot2::alpha("grey", 0.3),
                                         "CI" = ggplot2::alpha("purple3", 0.2)
                                       ),
                                       labels = c(
                                         CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
                                         expression(paste("1-sigma Error"))
                                       )
            ) +
            ggplot2::scale_colour_manual("",
                                         values = c(
                                           "black" = "black",
                                           "mean" = "purple3"
                                         ),
                                         labels = c("Data", "Posterior Fit")
            ) +
            ggplot2::guides(
              fill = ggplot2::guide_legend(override.aes = list(
                alpha = c(0.2, 0.4),
                size = 1
              )),
              colour = ggplot2::guide_legend(override.aes = list(
                linetype = c(0, 1),
                shape = c(16, NA),
                size = 2
              ))
            ) +
            ggplot2::facet_wrap(~SiteName)

          # Plot rate
          plot_rate <- ggplot2::ggplot() +
            ggplot2::geom_line(
              data = output_dataframes,
              ggplot2::aes(x = Age_BP, y = rate_pred, colour = "mean")
            ) +
            ggplot2::geom_ribbon(
              data = output_dataframes,
              ggplot2::aes(
                y = rate_pred,
                ymin = rate_lwr, ymax = rate_upr, x = Age_BP, fill = "CI"
              ), alpha = 0.3
            ) +
            ggplot2::theme_bw() +
            ggplot2::theme(
              strip.text.x = ggplot2::element_text(size = 7),
              strip.background = ggplot2::element_rect(fill = c("white"))
            ) +
            ggplot2::scale_fill_manual("",
                                       values = c(
                                         "CI" = ggplot2::alpha("purple3", 0.2)
                                       ),
                                       labels = c(
                                         CI = paste0(unique(output_dataframes$CI), " Credible Interval")
                                       )
            ) +
            ggplot2::scale_colour_manual("",
                                         values = c(
                                           "mean" = "purple3"
                                         ),
                                         labels = c("Posterior Fit")
            ) +
            ggplot2::guides(
              fill = ggplot2::guide_legend(override.aes = list(
                alpha = c(0.3), # , 0.4),
                size = 1
              )),
              colour = ggplot2::guide_legend(override.aes = list(
                linetype = c(1),
                shape = c(NA),
                size = 2
              ))
            ) +
            ggplot2::facet_wrap(~SiteName) +
            ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
            ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(size = 18, face = "bold"),
              axis.title = ggplot2::element_text(size = 12, face = "bold"),
              legend.text = ggplot2::element_text(size = 10)
            ) +
            ggplot2::labs(
              x = "Year (BP)",
              y = y_rate_lab,
              title = title,
              colour = ""
            )+
            ggplot2::geom_hline(yintercept = 0)

        }

        # Using informed caption or not
        if (plot_caption == TRUE) {
          plot_result <- plot_result + ggplot2::labs(caption = paste0(
            "Model type: Errors in Variables Integrated Gaussian Process Model \n No. proxy sites:", n_proxy,
            "\n No. tide gauge sites:", n_sites - n_proxy
          ))
          plot_rate <- plot_rate + ggplot2::labs(caption = paste0(
            "Model type: Errors in Variables Integrated Gaussian Process Model \n No. proxy sites:", n_proxy,
            "\n No. tide gauge sites:", n_sites - n_proxy
          ))
        } else {
          plot_result <- plot_result
          plot_rate <- plot_rate
        }

      if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
        plot_result <- plot_result +
          ggplot2::facet_wrap(~SiteName, scales = "free")
        plot_rate <- plot_rate +
          ggplot2::facet_wrap(~SiteName, scales = "free")
      }
    }

    output_plots <- list(plot_result = plot_result, plot_rate = plot_rate)
  }

  # NI spline time---------------
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

    # Plot model fit
    plot_result <- create_model_fit_plot(
      output_dataframes = output_dataframes,
      data = data,
      xlab = xlab,
      ylab = ylab,
      title = title
    )

    # Plot rate
    plot_rate <- create_rate_of_change_plot(
      output_dataframes = output_dataframes,
      data = data,
      xlab = xlab,
      y_rate_lab = y_rate_lab,
      title = title
    )

    # Age type BP
    if ("Age_type" %in% colnames(data)) {
      # Plot model fit
      plot_result <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = data, ggplot2::aes(
          xmin = Age_BP - Age_err, xmax = Age_BP + Age_err,
          ymin = RSL - RSL_err, ymax = RSL + RSL_err,
          fill = "Uncertainty",
        ), alpha = 0.7) +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(y = RSL, x = Age_BP, colour = "black"), size = 0.3
        ) +
        ggplot2::geom_line(
          data = output_dataframes,
          ggplot2::aes(x = Age_BP, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = output_dataframes,
          ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age_BP, fill = "CI"), alpha = 0.3
        ) +
        ggplot2::labs(
          x = "Year (BP)",
          y = "Relative Sea Level (m)",
          title = title, colour = ""
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 18, face = "bold"),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.text = ggplot2::element_text(size = 10)
        ) +
        ggplot2::scale_fill_manual("",
                                   values = c(
                                     "Uncertainty" = ggplot2::alpha("grey", 0.3),
                                     "CI" = ggplot2::alpha("purple3", 0.2)
                                   ),
                                   labels = c(
                                     CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
                                     expression(paste("1-sigma Error"))
                                   )
        ) +
        ggplot2::scale_colour_manual("",
                                     values = c(
                                       "black" = "black",
                                       "mean" = "purple3"
                                     ),
                                     labels = c("Data", "Posterior Fit")
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(override.aes = list(
            alpha = c(0.2, 0.4),
            size = 1
          )),
          colour = ggplot2::guide_legend(override.aes = list(
            linetype = c(0, 1),
            shape = c(16, NA),
            size = 2
          ))
        ) +
        ggplot2::facet_wrap(~SiteName)+
        ggplot2::scale_x_reverse()

      # Plot rate
      plot_rate <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = output_dataframes,
          ggplot2::aes(x = Age_BP, y = rate_pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = output_dataframes,
          ggplot2::aes(
            y = rate_pred,
            ymin = rate_lwr, ymax = rate_upr, x = Age_BP, fill = "CI"
          ), alpha = 0.3
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::scale_fill_manual("",
                                   values = c(
                                     "CI" = ggplot2::alpha("purple3", 0.2)
                                   ),
                                   labels = c(
                                     CI = paste0(unique(output_dataframes$CI), " Credible Interval")
                                   )
        ) +
        ggplot2::scale_colour_manual("",
                                     values = c(
                                       "mean" = "purple3"
                                     ),
                                     labels = c("Posterior Fit")
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(override.aes = list(
            alpha = c(0.3), # , 0.4),
            size = 1
          )),
          colour = ggplot2::guide_legend(override.aes = list(
            linetype = c(1),
            shape = c(NA),
            size = 2
          ))
        ) +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 18, face = "bold"),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.text = ggplot2::element_text(size = 10)
        ) +
        ggplot2::labs(
          x = "Year (BP)",
          y = y_rate_lab,
          title = title,
          colour = ""
        )+
        ggplot2::scale_x_reverse()+
        ggplot2::geom_hline(yintercept = 0)

    }


    # Plotting both tide gauge and proxy record
    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result +
        ggplot2::facet_wrap(~SiteName, scales = "free")
      plot_rate <- plot_rate +
        ggplot2::facet_wrap(~SiteName, scales = "free")
    }
    # Using caption or not
    if (plot_caption == TRUE) {
      plot_result <- plot_result + ggplot2::labs(caption = paste0(
        "Model type: Noisy Input Spline in Time \n No. proxy sites:", n_proxy,
        "\n No. tide gauge sites:", n_sites - n_proxy
      )
      )
      plot_rate <- plot_rate + ggplot2::labs(caption = paste0(
        "Model type: Noisy Input Spline in Time \n No. proxy sites:", n_proxy,
        "\n No. tide gauge sites:", n_sites - n_proxy)
      )
    }
    # Output plots
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

    # Plot model fit
    plot_result <- create_model_fit_plot(
      output_dataframes = output_dataframes,
      data = data,
      xlab = xlab,
      ylab = ylab,
      title = title
    )

    # Plot rate
    plot_rate <- create_rate_of_change_plot(
      output_dataframes = output_dataframes,
      data = data,
      xlab = xlab,
      y_rate_lab = y_rate_lab,
      title = title
    )

    # Age type BP
    if ("Age_type" %in% colnames(data)) {
      # Plot model fit
      plot_result <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = data, ggplot2::aes(
          xmin = Age_BP - Age_err, xmax = Age_BP + Age_err,
          ymin = RSL - RSL_err, ymax = RSL + RSL_err,
          fill = "Uncertainty",
        ), alpha = 0.7) +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(y = RSL, x = Age_BP, colour = "black"), size = 0.3
        ) +
        ggplot2::geom_line(
          data = output_dataframes,
          ggplot2::aes(x = Age_BP, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = output_dataframes,
          ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age_BP, fill = "CI"), alpha = 0.3
        ) +
        ggplot2::labs(
          x = "Year (BP)",
          y = "Relative Sea Level (m)",
          title = title, colour = ""
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 18, face = "bold"),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.text = ggplot2::element_text(size = 10)
        ) +
        ggplot2::scale_fill_manual("",
                                   values = c(
                                     "Uncertainty" = ggplot2::alpha("grey", 0.3),
                                     "CI" = ggplot2::alpha("purple3", 0.2)
                                   ),
                                   labels = c(
                                     CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
                                     expression(paste("1-sigma Error"))
                                   )
        ) +
        ggplot2::scale_colour_manual("",
                                     values = c(
                                       "black" = "black",
                                       "mean" = "purple3"
                                     ),
                                     labels = c("Data", "Posterior Fit")
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(override.aes = list(
            alpha = c(0.2, 0.4),
            size = 1
          )),
          colour = ggplot2::guide_legend(override.aes = list(
            linetype = c(0, 1),
            shape = c(16, NA),
            size = 2
          ))
        ) +
        ggplot2::facet_wrap(~SiteName)+
        ggplot2::scale_x_reverse()

      # Plot rate
      plot_rate <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = output_dataframes,
          ggplot2::aes(x = Age_BP, y = rate_pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = output_dataframes,
          ggplot2::aes(
            y = rate_pred,
            ymin = rate_lwr, ymax = rate_upr, x = Age_BP, fill = "CI"
          ), alpha = 0.3
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::scale_fill_manual("",
                                   values = c(
                                     "CI" = ggplot2::alpha("purple3", 0.2)
                                   ),
                                   labels = c(
                                     CI = paste0(unique(output_dataframes$CI), " Credible Interval")
                                   )
        ) +
        ggplot2::scale_colour_manual("",
                                     values = c(
                                       "mean" = "purple3"
                                     ),
                                     labels = c("Posterior Fit")
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(override.aes = list(
            alpha = c(0.3), # , 0.4),
            size = 1
          )),
          colour = ggplot2::guide_legend(override.aes = list(
            linetype = c(1),
            shape = c(NA),
            size = 2
          ))
        ) +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 18, face = "bold"),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.text = ggplot2::element_text(size = 10)
        ) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::labs(
          x = "Year (BP)",
          y = y_rate_lab,
          title = title,
          colour = ""
        )+
        ggplot2::scale_x_reverse()

    }


    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      plot_result <- plot_result +
        ggplot2::facet_wrap(~SiteName, scales = "free")
      plot_rate <- plot_rate +
        ggplot2::facet_wrap(~SiteName, scales = "free")
    }

    # Using caption or not
    if (plot_caption == TRUE) {
      # Plot model fit
      plot_result <- plot_result + ggplot2::labs(caption  = paste0(
        "Model type: Noisy Input Spline in Space Time \n No. proxy sites:", n_proxy,
        "\n No. tide gauge sites:", n_sites - n_proxy)
      )
      # Plot rate
      plot_rate <- plot_rate + ggplot2::labs(caption  = paste0(
        "Model type: Noisy Input Spline in Space Time \n No. proxy sites:", n_proxy,
        "\n No. tide gauge sites:", n_sites - n_proxy)
      )
    }

    # Output plots
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
    total_model_df <- output_dataframes$total_model_df %>%
       dplyr::mutate(data_type_id = as.factor(data_type_id))
    regional_component_df <- output_dataframes$regional_component_df %>%
      dplyr::mutate(data_type_id = as.factor(data_type_id))
    lin_loc_component_df <- output_dataframes$lin_loc_component_df %>%
      dplyr::mutate(data_type_id = as.factor(data_type_id))
    non_lin_loc_component_df <- output_dataframes$non_lin_loc_component_df %>%
      dplyr::mutate(data_type_id = as.factor(data_type_id))


    # Plotting proxy only
    if (plot_proxy_records == TRUE & plot_tide_gauges == FALSE) {
      data <- data %>%
        dplyr::filter(data_type_id == "ProxyRecord")
      total_model_df <- total_model_df %>%
        dplyr::filter(data_type_id == "ProxyRecord")

      regional_component_df <- regional_component_df %>%
        dplyr::filter(data_type_id == "ProxyRecord")

      lin_loc_component_df <- lin_loc_component_df %>%
        dplyr::filter(data_type_id == "ProxyRecord")
      non_lin_loc_component_df <- non_lin_loc_component_df %>%
        dplyr::filter(data_type_id == "ProxyRecord")
    }
    # Plotting tide gauge only
    if (plot_proxy_records == FALSE & plot_tide_gauges == TRUE) {
      data <- data %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      total_model_df <- total_model_df %>%
        dplyr::filter(data_type_id == "TideGaugeData")

      regional_component_df <- regional_component_df

      lin_loc_component_df <- lin_loc_component_df %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      non_lin_loc_component_df <- non_lin_loc_component_df %>%
        dplyr::filter(data_type_id == "TideGaugeData")

    }
      # Plot model fit
      plot_result <- create_model_fit_plot(
        output_dataframes = total_model_df,
        data = data,
        xlab = xlab,
        ylab = ylab,
        title = title
      )

      # Plot rate
      plot_rate <- create_rate_of_change_plot(
        output_dataframes = total_model_df,
        data = data,
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
        ggplot2::labs(y = "Sea Level (m)", x = "Year (CE)")

      # Derivative of Predicted Regional Component Plot---------------------------
      regional_rate_plot <-
        ggplot2::ggplot() +
        ggplot2::geom_line(
          data = regional_component_df,
          ggplot2::aes(x = Age, y = rate_pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = regional_component_df,
          ggplot2::aes(ymin = rate_lwr, ymax = rate_upr, x = Age, fill = "CI"), alpha = 0.2
        ) +
        ggplot2::theme_bw() +
        ggplot2::geom_hline(yintercept = 0) +
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
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 15),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          axis.text = ggplot2::element_text(size = 12),
          legend.text = ggplot2::element_text(size = 12),
          strip.text.x = ggplot2::element_text(size = 10),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::labs(caption = "",colour = "",
                      x = "Year (CE)",y = "Rate of Change (mm/yr)") +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom")

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
        ggplot2::labs(y = "Sea Level (m)",x ="Year (CE)", colour = "") +
        ggplot2::facet_wrap(~SiteName)+
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom")

      # Plot Non linear local plot
      non_lin_loc_plot <-
        ggplot2::ggplot() +
        ggplot2::geom_line(
          data = non_lin_loc_component_df,
          ggplot2::aes(x = Age, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = non_lin_loc_component_df,
          ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age, fill = "CI"), alpha = 0.3
        ) +
        ggplot2::labs(
          x = xlab,
          y = "Sea Level (m)",
          title = title,
          colour = ""
        ) +
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
        ggplot2::scale_fill_manual("",
                                   values = c(
                                     "CI" = ggplot2::alpha("#ad4c14", 0.2)
                                   ),
                                   labels = c(
                                     CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
                                     expression(paste("1-sigma Error"))
                                   )
        ) +
        ggplot2::scale_colour_manual("",
                                     values = c(
                                       "mean" = "#ad4c14"
                                     ),
                                     labels = c("Posterior Fit")
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(override.aes = list(
            alpha = c(0.2),
            size = 1
          )),
          colour = ggplot2::guide_legend(override.aes = list(
            linetype = c(1),
            size = 2
          ))
        ) +
        ggplot2::facet_wrap(~SiteName)

      # Plotting rate of non linear component
      non_lin_loc_rate_plot <- create_rate_of_change_plot(
        output_dataframes = non_lin_loc_component_df,
        data = data,
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
          data = total_model_df,
          ggplot2::aes(x = Age, y = pred, colour = "Total Posterior Model") # ID)
        ) +
        ggplot2::geom_ribbon(
          data = total_model_df,
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
        ggplot2::labs(y = "Sea Level (m)",x = "Year (CE)", colour = "") +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom")

      # Age type BP
    if ("Age_type" %in% colnames(data)) {
        # Plot result
        plot_result <- ggplot2::ggplot() +
          ggplot2::geom_rect(data = data, ggplot2::aes(
            xmin = Age_BP - Age_err, xmax = Age_BP + Age_err,
            ymin = RSL - RSL_err, ymax = RSL + RSL_err,
            fill = "Uncertainty",
          ), alpha = 0.7) +
          ggplot2::geom_point(
            data = data,
            ggplot2::aes(y = RSL, x = Age_BP, colour = "black"), size = 0.3
          ) +
          ggplot2::geom_line(
            data = total_model_df,
            ggplot2::aes(x = Age_BP, y = pred, colour = "mean")
          ) +
          ggplot2::geom_ribbon(
            data = total_model_df,
            ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age_BP, fill = "CI"), alpha = 0.3
          ) +
          ggplot2::scale_fill_manual("",
                                     values = c(
                                       "Uncertainty" = ggplot2::alpha("grey", 0.3),
                                       "CI" = ggplot2::alpha("purple3", 0.2)
                                     ),
                                     labels = c(
                                       CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
                                       Uncertainty = expression(paste("1-sigma Error"))
                                     )
          ) +
          ggplot2::scale_colour_manual("",
                                       values = c(
                                         "black" = "black",
                                         "mean" = "purple3"
                                       ),
                                       labels = c("Data", "Posterior Fit")
          ) +
          ggplot2::guides(
            fill = ggplot2::guide_legend(override.aes = list(
              alpha = c(0.3, 0.2),
              size = 1
            )),
            colour = ggplot2::guide_legend(override.aes = list(
              linetype = c(0, 1),
              shape = c(16, NA),
              size = 2
            ))
          ) +
          ggplot2::facet_wrap(~SiteName) +
          ggplot2::labs(x = "Year (BP)", y = ylab, title = title, colour = "") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            strip.text.x = ggplot2::element_text(size = 7),
            strip.background = ggplot2::element_rect(fill = c("white"))
          ) +
          ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
          ggplot2::scale_x_reverse()

        # Rate
        plot_rate <-ggplot2::ggplot() +
          ggplot2::geom_line(
            data = total_model_df,
            ggplot2::aes(x = Age_BP, y = rate_pred, colour = "mean")
          ) +
          ggplot2::geom_ribbon(
            data = total_model_df,
            ggplot2::aes(
              y = rate_pred,
              ymin = rate_lwr, ymax = rate_upr, x = Age_BP, fill = "CI"
            ), alpha = 0.3
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            strip.text.x = ggplot2::element_text(size = 7),
            strip.background = ggplot2::element_rect(fill = c("white"))
          ) +
          ggplot2::scale_fill_manual("",
                                     values = c(
                                       "CI" = ggplot2::alpha("purple3", 0.2)
                                     ),
                                     labels = c(
                                       CI = paste0(unique(output_dataframes$CI), " Credible Interval")
                                     )
          ) +
          ggplot2::scale_colour_manual("",
                                       values = c(
                                         "mean" = "purple3"
                                       ),
                                       labels = c("Posterior Fit")
          ) +
          ggplot2::guides(
            fill = ggplot2::guide_legend(override.aes = list(
              alpha = c(0.3),
              size = 1
            )),
            colour = ggplot2::guide_legend(override.aes = list(
              linetype = c(1),
              shape = c(NA),
              size = 2
            ))
          ) +
          ggplot2::facet_wrap(~SiteName) +
          ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
          ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 18, face = "bold"),
            axis.title = ggplot2::element_text(size = 12, face = "bold"),
            legend.text = ggplot2::element_text(size = 10)
          ) +
          ggplot2::labs(
            x = "Year (BP)",
            y = y_rate_lab,
            title = title,
            colour = ""
          )+
          ggplot2::geom_hline(yintercept = 0)+
          ggplot2::scale_x_reverse()

        # Regional
        regional_plot <- ggplot2::ggplot() +
          ggplot2::geom_line(
            data = regional_component_df,
            ggplot2::aes(x = Age_BP, y = pred, colour = "mean")
          ) +
          ggplot2::geom_ribbon(
            data = regional_component_df,
            ggplot2::aes(ymin = lwr, ymax = upr, x = Age_BP, fill = "CI"), alpha = 0.2
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
          ggplot2::scale_x_reverse() +
          ggplot2::labs(x = "Year (BP)",y = "Sea Level (m)")

        # Regional rate
        regional_rate_plot <- ggplot2::ggplot() +
          ggplot2::geom_line(
            data = regional_component_df,
            ggplot2::aes(x = Age_BP, y = rate_pred, colour = "mean")
          ) +
          ggplot2::geom_ribbon(
            data = regional_component_df,
            ggplot2::aes(ymin = rate_lwr, ymax = rate_upr, x = Age_BP, fill = "CI"), alpha = 0.2
          ) +
          ggplot2::theme_bw() +
          ggplot2::geom_hline(yintercept = 0) +
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
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 15),
            axis.title = ggplot2::element_text(size = 12, face = "bold"),
            axis.text = ggplot2::element_text(size = 12),
            legend.text = ggplot2::element_text(size = 12),
            strip.text.x = ggplot2::element_text(size = 10),
            strip.background = ggplot2::element_rect(fill = c("white"))
          ) +
          ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
          ggplot2::labs(colour = "",x ="Year (BP)",y = "Rate of Change (mm/yr)")+
          ggplot2::scale_x_reverse()

        # Linear Plot
        lin_loc_plot <-  ggplot2::ggplot() +
          ggplot2::geom_line(
            data = lin_loc_component_df,
            ggplot2::aes(x = Age_BP, y = pred, colour = "mean")
          ) +
          ggplot2::geom_ribbon(
            data = lin_loc_component_df,
            ggplot2::aes(
              ymin = lwr, ymax = upr, x = Age_BP,
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
          ggplot2::facet_wrap(~SiteName) +
          ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
          ggplot2::labs(colour = "",x = "Year (BP)",y = "Sea Level (m)")+
          ggplot2::scale_x_reverse()
        # Non linear
        non_lin_loc_plot <-
          ggplot2::ggplot() +
        ggplot2::geom_line(
          data = non_lin_loc_component_df,
          ggplot2::aes(x = Age_BP, y = pred, colour = "mean")
        ) +
        ggplot2::geom_ribbon(
          data = non_lin_loc_component_df,
          ggplot2::aes(y = pred, ymin = lwr, ymax = upr, x = Age_BP, fill = "CI"), alpha = 0.3
        ) +
        ggplot2::labs(
          x = xlab,
          y = "Sea Level (m)",
          title = title,
          colour = ""
        ) +
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
        ggplot2::scale_fill_manual("",
                                   values = c(
                                     "CI" = ggplot2::alpha("#ad4c14", 0.2)
                                   ),
                                   labels = c(
                                     CI = paste0(unique(output_dataframes$CI), " Credible Interval"),
                                     expression(paste("1-sigma Error"))
                                   )
        ) +
        ggplot2::scale_colour_manual("",
                                     values = c(
                                       "mean" = "#ad4c14"
                                     ),
                                     labels = c("Posterior Fit")
        ) +
          ggplot2::guides(
            fill = ggplot2::guide_legend(override.aes = list(
              alpha = c(0.2),
              size = 1
            )),
            colour = ggplot2::guide_legend(override.aes = list(
              linetype = c(1),
              size = 2
            ))
          ) +
          ggplot2::facet_wrap(~SiteName)+
          ggplot2::scale_x_reverse()

        # Non linear local rate
        non_lin_loc_rate_plot <-
          ggplot2::ggplot() +
          ggplot2::geom_line(
            data = non_lin_loc_component_df,
            ggplot2::aes(x = Age_BP, y = rate_pred, colour = "mean")
          ) +
          ggplot2::geom_ribbon(
            data = non_lin_loc_component_df,
            ggplot2::aes(y = rate_pred, ymin = rate_lwr, ymax = rate_upr, x = Age_BP, fill = "CI"), alpha = 0.2
          ) +
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
          ggplot2::scale_fill_manual("",
                                     values = c(
                                       "CI" = ggplot2::alpha("#ad4c14", 0.2)
                                     ),
                                     labels = c(
                                       CI = paste0(unique(output_dataframes$CI), " Credible Interval")
                                     )
          ) +
          ggplot2::scale_colour_manual("",
                                       values = c("mean" = "#ad4c14"),
                                       labels = c("Posterior Fit")
          ) +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::guides(
            fill = ggplot2::guide_legend(override.aes = list(
              alpha = c(0.4),
              size = 1
            )),
            colour = ggplot2::guide_legend(override.aes = list(
              linetype = c(1),
              shape = c(NA),
              size = 2
            ))
          ) +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::facet_wrap(~SiteName)+
          ggplot2::labs(colour = "",
                        x = "Year (BP)",
                        y = "Rate of Change (mm/year)")+
          ggplot2::scale_x_reverse()

        # All components
        all_components_CI_plot <- ggplot2::ggplot() +
          # Linear Local Component + site specific vertical offset
          ggplot2::geom_line(
            data = lin_loc_component_df,
            ggplot2::aes(x = Age_BP, y = pred, colour = "Site Specific vertical offset + \n Linear Local Component")
          ) +
          ggplot2::geom_ribbon(
            data = lin_loc_component_df,
            ggplot2::aes(ymin = lwr, ymax = upr, x = Age_BP, fill = "Site Specific vertical offset + \n Linear Local Component"),
            alpha = 0.3
          ) +

          # Non linear Local
          ggplot2::geom_line(
            data = non_lin_loc_component_df,
            ggplot2::aes(x = Age_BP, y = pred, colour = "Non Linear Local Component")
          ) +
          ggplot2::geom_ribbon(
            data = non_lin_loc_component_df,
            ggplot2::aes(ymin = lwr, ymax = upr, x = Age_BP, fill = "Non Linear Local Component"),
            alpha = 0.3
          ) +
          # Regional Component
          ggplot2::geom_line(
            data = regional_component_df,
            ggplot2::aes(x = Age_BP, y = pred, colour = "Regional Component")
          ) +
          ggplot2::geom_ribbon(
            data = regional_component_df,
            ggplot2::aes(ymin = lwr, ymax = upr, x = Age_BP, fill = "Regional Component"),
            alpha = 0.3
          ) +
          # Total Model
          ggplot2::geom_line(
            data = total_model_df,
            ggplot2::aes(x = Age_BP, y = pred, colour = "Total Posterior Model")
          ) +
          ggplot2::geom_ribbon(
            data = total_model_df,
            ggplot2::aes(ymin = lwr, ymax = upr, x = Age_BP, fill = "Total Posterior Model"),
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
          ggplot2::facet_wrap(~SiteName) +
          ggplot2::labs(x = "Year (BE)", y = "Sea Level (m)", colour = "") +
          ggplot2::theme(legend.box = "horizontal", legend.position = "bottom")+
          ggplot2::scale_x_reverse()
    }

    # Using caption or not
    if (plot_caption == TRUE) {
      # Plot total model fit
      plot_result <- plot_result + ggplot2::labs(caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        )
      )

      # Plot total model rate of change plot
      plot_rate <- plot_rate + ggplot2::labs(caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy))
      # Regional Component Plot---------------------------
      regional_plot <- regional_plot + ggplot2::labs(caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy))

      # Derivative of Predicted Regional Component Plot---------------------------
      regional_rate_plot <-
        regional_rate_plot +
        ggplot2::labs(caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ))

      # Linear Local + Site specific vertical offset ---------------------
      lin_loc_plot <-lin_loc_plot+
        ggplot2::labs(caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ))

      # Plot Non linear local plot
      non_lin_loc_plot <-non_lin_loc_plot+
        ggplot2::labs(caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ))


      # Plot rate for non linear local
      non_lin_loc_rate_plot <- non_lin_loc_rate_plot + ggplot2::labs(caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ))


      # Separate Components on one plot with CI --------
      all_components_CI_plot <- all_components_CI_plot +
        ggplot2::labs(caption = paste0(
          "Model type: Noisy Input GAM for signal decomposition \n No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ))
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

    # Output plots
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

# Plotting individual plots
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
