#' Plot raw data with measurement uncertainty.
#'
#' In this function, the raw data is plotted with grey uncertainty boxes representing the uncertainty associated with the input and the output.
#' The function allows the user to plot the proxy record data and tide gauge data together or separately.
#'
#'
#' @param x An object created via the function \code{\link{reslr_load}}
#' @param title Title of the plot
#' @param xlab Labeling the x-axis
#' @param ylab Labeling the y-axis
#' @param plot_proxy_records Plotting the proxy records on their own and this is the default
#' @param plot_tide_gauges Plotting the tide gauge data with the proxy records
#' @param plot_caption Plotting an informed caption with the number of tide gauges and proxy sites.
#' @param ...  Not used
#'
#' @return Plot of the raw data with the measurement uncertainty.
#' @export
#'
#' @examples
#' full_dataset <- reslr_load(NAACproxydata)
#' plot(full_dataset)
plot.reslr_input <- function(x,
                             title = "",
                             xlab = "Year (CE)",
                             ylab = "Relative Sea Level (m)",
                             plot_tide_gauges = FALSE,
                             plot_proxy_records = TRUE,
                             plot_caption = TRUE,
                             ...) {
  Age <- RSL <- Age_err <- RSL_err <- SiteName <- data_type_id <-y_lwr_box <- y_upr_box <- SL <- obs_index <-  NULL
  # Input data-------
  input_data <- x
  data <- input_data$data
  data_grid <- input_data$data_grid
  n_sites <- length(data$SiteName %>% unique())
  n_proxy <- data %>%
    dplyr::filter(data_type_id == "ProxyRecord") %>%
    dplyr::select(SiteName, data_type_id) %>%
    unique() %>%
    nrow()
  # Plotting only Proxy Record
  if (plot_proxy_records == TRUE & plot_tide_gauges == FALSE) {
    data <- data %>%
      dplyr::filter(data_type_id == "ProxyRecord")
  }
  # Plotting tide gauge only
  if (plot_proxy_records == FALSE & plot_tide_gauges == TRUE) {
    data <- data %>%
      dplyr::filter(data_type_id == "TideGaugeData")
  }
    # Raw data plot
    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = data, ggplot2::aes(
        xmin = Age - Age_err, xmax = Age + Age_err,
        ymin = RSL - RSL_err, ymax = RSL + RSL_err,
        fill = "gray",
      ), alpha = 0.7) +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(y = RSL, x = Age, colour = "black"), size = 0.3
      ) +
      ggplot2::labs(x = xlab, y = ylab, title = title) +
      ggplot2::theme_bw() +
      ggplot2::labs(colour = "") +
      ggplot2::theme(
        strip.text.x = ggplot2::element_text(size = 7),
        strip.background = ggplot2::element_rect(fill = c("white"))
      ) +
      ggplot2::scale_fill_manual("",
        values = "grey",
        labels = expression(paste("1-sigma Error")),
        guide = ggplot2::guide_legend(override.aes = list(alpha = 0.7))
      ) +
      ggplot2::scale_colour_manual(
        values = c("black"),
        labels = c("Data")
      ) +
      ggplot2::facet_wrap(~SiteName) +
      ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        legend.text = ggplot2::element_text(size = 10)
      )
    # Plotting when Age type is BP
    if("Age_type" %in% colnames(data)){
      p <- ggplot2::ggplot()+
        ggplot2::geom_rect(data = data, ggplot2::aes(
          xmin = Age_BP - Age_err, xmax = Age_BP + Age_err,
          ymin = RSL - RSL_err, ymax = RSL + RSL_err,
          fill = "gray",
        ), alpha = 0.7) +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(y = RSL, x = Age_BP, colour = "black"), size = 0.3
        ) +
        ggplot2::labs(x = "Year (BP)", y = ylab, title = title) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "") +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::scale_fill_manual("",
                                   values = "grey",
                                   labels = expression(paste("1-sigma Error")),
                                   guide = ggplot2::guide_legend(override.aes = list(alpha = 0.7))
        ) +
        ggplot2::scale_colour_manual(
          values = c("black"),
          labels = c("Data")
        ) +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 18, face = "bold"),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.text = ggplot2::element_text(size = 10)
        )+
        ggplot2::scale_x_reverse()
    }
    else{
      p <- p
    }
    # Plotting both TG & proxy
    if (plot_proxy_records == TRUE & plot_tide_gauges == TRUE) {
      p <- p + ggplot2::facet_wrap(~SiteName, scales = "free")
    }

    # Informed caption
    if (plot_caption == TRUE) {
      p <- p + ggplot2::labs(caption = paste0(
        "No. proxy sites:", n_proxy,
        "\n No. tide gauge sites:", n_sites - n_proxy
      ))
    } else {
      p <- p
    }


  # Plotting detrend data put into 1 site only ------------------
  if (inherits(x, "detrend_data") == TRUE) {
    detrend_data_un_box <- input_data$detrend_data_un_box
    # Plotting tide gauges only
    if (plot_tide_gauges == TRUE & plot_proxy_records == FALSE) {
      data <- data %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      detrend_data_un_box <- detrend_data_un_box %>%
        dplyr::filter(data_type_id == "TideGaugeData")
      }

    # Plotting proxy records only
    if (plot_tide_gauges == FALSE & plot_proxy_records == TRUE) {
      data <- data %>%
        dplyr::filter(data_type_id == "ProxyRecord")

      detrend_data_un_box <- detrend_data_un_box %>%
        dplyr::filter(data_type_id == "ProxyRecord")
    }

      # Plot result
      p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = detrend_data_un_box,
                              ggplot2::aes(x = Age*1000, y = SL,
                                           group = obs_index,fill = "gray"),alpha = 0.5)+
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(y = SL, x = Age, colour = "black"), size = 0.3
        ) +
        ggplot2::labs(x = xlab, y = "Sea Level (m)", title = title) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "") +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::scale_fill_manual("",
                                   values = "grey",
                                   labels = expression(paste("1-sigma Error")),
                                   guide = ggplot2::guide_legend(override.aes = list(alpha = 0.7))
        ) +
        ggplot2::scale_colour_manual(
          values = c("black"),
          labels = c("Data")
        ) +
        ggplot2::facet_wrap(~SiteName) +
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 18, face = "bold"),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.text = ggplot2::element_text(size = 10)
        )



    # Age type BP
    if("Age_type" %in% colnames(data)){
      p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = detrend_data_un_box,
                              ggplot2::aes(x = Age_BP*1000,
                                           y = SL,group = obs_index,fill = "gray"),alpha = 0.5)+
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(y = SL, x = Age_BP, colour = "black"), size = 0.3
        ) +
        ggplot2::labs(x = "Year (BP)", y = "Sea Level (m)", title = title) +
        ggplot2::theme_bw() +
        ggplot2::labs(colour = "") +
        ggplot2::theme(
          strip.text.x = ggplot2::element_text(size = 7),
          strip.background = ggplot2::element_rect(fill = c("white"))
        ) +
        ggplot2::scale_fill_manual("",
                                   values = "grey",
                                   labels = expression(paste("1-sigma Error")),
                                   guide = ggplot2::guide_legend(override.aes = list(alpha = 0.7))
        ) +
        ggplot2::scale_colour_manual(
          values = c("black"),
          labels = c("Data")
        ) +
        ggplot2::scale_x_reverse()+
        ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
        ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 18, face = "bold"),
          axis.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.text = ggplot2::element_text(size = 10)
        )
    }
    else{
      p <- p
    }
    # Plotting tide gauges & proxy records together
    if (plot_tide_gauges == TRUE & plot_proxy_records == TRUE) {
        p <- p + ggplot2::facet_wrap(~SiteName, scales = "free")
    }

    # If plotting informed caption
    if (plot_caption == TRUE) {
      p <- p + ggplot2::labs(caption = paste0(
        "No. proxy sites:", n_proxy,
        "\n No. tide gauge sites:", n_sites - n_proxy
      ))
    }
    else {
      p <- p
    }

}

  return(p)
}
