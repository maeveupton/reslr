#' Plot raw data with measurement uncertainty.
#'
#' For all models except the NI GAM decomposition, additional tide gauge data is not recommended. However, using this function the user has the ability to plot the tide gauge data with the proxy data.
#' For the NI GAM decomposition, additional tide gauge data is recommended and this function will create plots of the proxy sites with their uncertainty boxes and the tide gauge sites.
#' For the NI spline in space time, additional tide gauge data can be used but it will needed to be examined by the user.
#' If tide gauges are used in the model, the user has the ability plot the output with or without the inclusion of the tide gauges.
#'
#' @param x An object created via the function \code{\link{reslr_load}}
#' @param title Title of the Plot
#' @param xlab Labeling the x-axis
#' @param ylab Labeling the y-axis
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
                             title = "Plot of the raw data",
                             xlab = "Year (CE)",
                             ylab = "Y axis", # "Relative Sea Level (m)",
                             plot_tide_gauges = FALSE,
                             plot_caption = TRUE,
                             ...) {
  Age <- RSL <- Age_err <- RSL_err <- SiteName <- data_type_id <- NULL
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

  # Plotting both tide gauge and proxy record
  if (plot_tide_gauges == TRUE) {
      p <- ggplot2::ggplot() +
        ggplot2::geom_rect(
          data = data,
          ggplot2::aes(
            xmin = Age * 1000 - Age_err * 1000,
            xmax = Age * 1000 + Age_err * 1000,
            ymin = RSL - RSL_err, ymax = RSL + RSL_err,
            fill = "gray",
          ), alpha = 0.7
        ) +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.3
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
      # Plotting informed caption
      if(plot_caption == TRUE){
        p <- p +
        ggplot2::labs(caption = paste0(
          "No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ))
      }
    # No caption
    else {
      p <- p
    }
  }
  else {
    # Plotting only Proxy Record
    data <- data %>%
      dplyr::filter(data_type_id == "ProxyRecord")
    # Plot
      p <- ggplot2::ggplot() +
        ggplot2::geom_rect(data = data, ggplot2::aes(
          xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
          ymin = RSL - RSL_err, ymax = RSL + RSL_err,
          fill = "gray",
        ), alpha = 0.7) +
        ggplot2::geom_point(
          data = data,
          ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.3
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
      if (plot_caption == TRUE) {
        p <- p + ggplot2::labs(caption = paste0(
          "No. proxy sites:", n_proxy,
          "\n No. tide gauge sites:", n_sites - n_proxy
        ))
    }
      else {
        p <-p
      }
  }
  if (inherits(x, "detrend_data") == TRUE) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_rect(data = data, ggplot2::aes(
        xmin = Age * 1000 - Age_err * 1000,
        xmax = Age * 1000 + Age_err * 1000,
        ymin = y_lwr_box,
        ymax = y_upr_box,
        fill = "gray",
      ), alpha = 0.7) +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(y = SL, x = Age * 1000, colour = "black"), size = 0.3
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

    # If plotting informed caption
    if (plot_caption == TRUE) {
      p <- p + ggplot2::labs(caption = paste0(
        "No. proxy sites:", n_proxy # ,
        # "\n No. tide gauge sites:", n_sites - n_proxy
      ))
    } else {
      p <- p
    }
  }

  return(p)
}
