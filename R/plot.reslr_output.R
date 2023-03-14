#' Plotting the results for Noisy Input GAM decomposition
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
                        plot_tide_gauges = FALSE,
                        ...) {
  Age <- RSL <- time_deriv_component_post_df <- mod_output_pred_df<-Age_err <- ID <- RSL_err <- lwr <- upr <- lwr_50 <- lwr_95 <- upr_50 <- upr_95 <- rate_y <- rate_lwr_95 <- rate_upr_95 <- SiteName <- data_type_id <- pred_y <- NULL
  # Not plotting the tide gauge data -------------
  # if (("data_type_id" %in% colnames(jags_output$data)) & plot_tide_gauges == FALSE) {
  #   jags_output$data <- jags_output$data %>%
  #     dplyr::mutate(data_type_id = as.factor(data_type_id)) %>%
  #     dplyr::filter(data_type_id == "ProxyRecordData")
  #   jags_output$predict_data <- jags_output$predict_data %>%
  #     dplyr::mutate(data_type_id = as.factor(data_type_id)) %>%
  #     dplyr::filter(data_type_id == "ProxyRecordData")
  # } else {
  #   data <- data
  # }
  #if(inherits(jags_output,"reslr_output") == TRUE){
  jags_output <- x
  # EIV slr------------
  if (inherits(jags_output, "eiv_slr_t") == TRUE) {
    total_model_df <- parameter_estimate(jags_output = jags_output)$output_dataframes$total_model_df
    plot_result <-
      ggplot2::ggplot() +
      ggplot2::geom_rect(data = jags_output$data, ggplot2::aes(
        xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
        ymin = RSL - RSL_err, ymax = RSL + RSL_err, fill = "Uncertainty",
      ), alpha = 0.4) +
      ggplot2::geom_point(
        data = jags_output$data,
        ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.5
      ) +
      ggplot2::geom_line(
        data = total_model_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_df,
        ggplot2::aes(y = RSL, ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = total_model_df,
      #   ggplot2::aes(y = RSL, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
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
      )
    message("Plotted EIV Simple linear regression")

    output_plots <- list(plot_result = plot_result)
  }

  # EIV CP 1
  if (inherits(jags_output, "eiv_cp1_t")) {
    total_model_df <- parameter_estimate(jags_output = jags_output)$output_dataframes$total_model_df
    plot_result <-
      ggplot2::ggplot() +
      ggplot2::geom_rect(data = jags_output$data, ggplot2::aes(
        xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
        ymin = RSL - RSL_err, ymax = RSL + RSL_err, fill = "Uncertainty",
      ), alpha = 0.4) +
      ggplot2::geom_point(
        data = jags_output$data,
        ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.5
      ) +
      ggplot2::geom_line(
        data = total_model_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_df,
        ggplot2::aes(y = RSL, ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = total_model_df,
      #   ggplot2::aes(y = RSL, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
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
          "Uncertainty" = ggplot2::alpha("grey", 0.4),
          "95" = ggplot2::alpha("purple3", 0.2)
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
      )
    message("Plotted 1 Change Point")

    output_plots <- list(plot_result = plot_result)
  }

  # EIV CP2
  if (inherits(jags_output, "eiv_cp2_t")) {
    total_model_df <- parameter_estimate(jags_output = jags_output)$output_dataframes$total_model_df
    plot_result <-
      ggplot2::ggplot() +
      ggplot2::geom_rect(data = jags_output$data, ggplot2::aes(
        xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
        ymin = RSL - RSL_err, ymax = RSL + RSL_err, fill = "Uncertainty",
      ), alpha = 0.4) +
      ggplot2::geom_point(
        data = jags_output$data,
        ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.5
      ) +
      ggplot2::geom_line(
        data = total_model_df, # jags_output$mod_output_pred_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_df,
        ggplot2::aes(y = RSL, ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = total_model_df,
      #   ggplot2::aes(y = RSL, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
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
          "Uncertainty" = ggplot2::alpha("grey", 0.4),
          "95" = ggplot2::alpha("purple3", 0.2)
          # "50" = ggplot2::alpha("purple3", 0.3)
        ),
        labels = c(
          expression(paste("1-sigma error")),
          "95% Credible Interval" # , "50% Credible Interval"
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
      )

    message("Plotted a 2 Change Point Model")

    output_plots <- list(plot_result = plot_result)
  }

  # EIV CP 3
  if (inherits(jags_output, "eiv_cp3_t")) {
    total_model_df <- parameter_estimate(jags_output = jags_output)$output_dataframes$total_model_df
    plot_result <-
      ggplot2::ggplot() +
      ggplot2::geom_rect(data = jags_output$data, ggplot2::aes(
        xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
        ymin = RSL - RSL_err, ymax = RSL + RSL_err, fill = "Uncertainty",
      ), alpha = 0.4) +
      ggplot2::geom_point(
        data = jags_output$data,
        ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.5
      ) +
      ggplot2::geom_line(
        data = total_model_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_df,
        ggplot2::aes(y = RSL, ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = total_model_df,
      #   ggplot2::aes(y = RSL, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
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
      # ggplot2::theme(
      #   legend.position = c(0.95, 0.01),
      #   legend.justification = c(1, 0),
      #   legend.spacing.y = ggplot2::unit(0.1, "cm"),
      #   legend.title = ggplot2::element_blank(),
      #   legend.margin = ggplot2::margin(c(1, 1, 1, 1))
      # ) +
      ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
      ggplot2::labs(colour = "") +
      ggplot2::scale_fill_manual("",
        values = c(
          "Uncertainty" = ggplot2::alpha("grey", 0.4),
          "95" = ggplot2::alpha("purple3", 0.2)
        ),
        # "50" = ggplot2::alpha("purple3", 0.3)
        labels = c(
          "95% Credible Interval", # "50% Credible Interval",
          expression(paste("1-sigma error"))
        )
      ) +
      ggplot2::scale_colour_manual("",
        values = c(
          "black" = "black",
          "mean" = "purple3"
        ),
        labels = c("Proxy Records", "Posterior Fit")
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
      )
    message("Plotted 3 Change Point")

    output_plots <- list(plot_result = plot_result)
  }

  # EIV IGP
  if (inherits(jags_output, "eiv_igp_t")) {
    ## get model based estimates
    res <- parameter_estimate(jags_output)
    pred_res <- res$output_dataframes
    # pred_res$model_label <- ifelse(mod1$EIV,paste0("eiv-",mod1$model),mod1$model)
    # BP_scale <- mod1$BP_scale

    ### plot the results
    plot_result <- ggplot2::ggplot(pred_res, ggplot2::aes(x = t, y = pred_y)) +
      # ggplot2::geom_point(data = jags_output$data,
      #                     ggplot2::aes(y = RSL, x = Age*1000), size = 0.5) +
      # ggplot2::geom_rect(data = jags_output$data, ggplot2::aes(
      #   xmin = Age*1000 - Age_err*1000, xmax = Age*1000 + Age_err*1000,
      #   ymin = RSL - RSL_err, ymax = RSL + RSL_err
      # ), alpha = 0.2) +
      ggplot2::geom_line(ggplot2::aes()) + # colour = model_label
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr_95, ymax = upr_95), alpha = 0.4) + # fill = model_label
      # ggplot2::geom_polygon(ggplot2::aes(x = x, y = y, group = obs_index), data = data_to_plot, alpha = 0.2) +
      ggplot2::ylab("Relative Sea Level (m)") +
      ggplot2::xlab("Age (CE)") +
      ggplot2::labs(colour = "", fill = "95% UI") +
      ggplot2::theme_classic()

    plot_rate <- ggplot2::ggplot(pred_res %>% tidyr::drop_na(), ggplot2::aes(x = t, y = rate_y)) +
      ggplot2::geom_line() + # colour = model_label ggplot2::aes()fill = model_label
      ggplot2::geom_ribbon(ggplot2::aes(ymin = rate_lwr_95, ymax = rate_upr_95), alpha = 0.4) +
      ggplot2::ylab("Rate of change (mm/yr)") +
      ggplot2::xlab("Age (CE)") +
      ggplot2::labs(colour = "", fill = "95% UI") +
      ggplot2::theme_classic()

    message("Plotted EIV-IGP model & rate")
    output_plots <- list(plot_result = plot_result, plot_rate = plot_rate)
  }

  # NIGAM time
  if (inherits(jags_output, "ni_spline_t") == TRUE) {
    # Dataframes to plot
    output_dataframes <- parameter_estimate(jags_output = jags_output)$output_dataframes
    total_model_df <- output_dataframes$total_model_df
    total_model_rate_df <- output_dataframes$total_model_rate_df

    # Plots
    plot_result <-
      ggplot2::ggplot() +
      ggplot2::geom_rect(data = jags_output$data, ggplot2::aes(
        xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
        ymin = RSL - RSL_err, ymax = RSL + RSL_err, fill = "Uncertainty",
      ), alpha = 0.4) +
      ggplot2::geom_point(
        data = jags_output$data,
        ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.5
      ) +
      ggplot2::geom_line(
        data = total_model_df, # jags_output$mod_output_pred_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_df,
        ggplot2::aes(y = RSL, ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = total_model_df,
      #   ggplot2::aes(y = RSL, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
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
      )

    message("Plotted NIGAM in time using data")

    # Plotting Rate of Change for Total component----------
    plot_rate <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = total_model_rate_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_rate_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = total_model_rate_df,
        ggplot2::aes(ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
      ) +
      ggplot2::theme_bw() +
      ggplot2::ylab("Rate of change (mm/yr)") +
      ggplot2::xlab("Age (CE)") +
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
      ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
      ggplot2::labs(colour = "") +
      ggplot2::scale_fill_manual("",
        values = c(
          "95" = ggplot2::alpha("purple3", 0.2),
          "50" = ggplot2::alpha("purple3", 0.3)
        ),
        labels = c(
          "95% Credible Interval",
          "50% Credible Interval"
        )
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

    message("Plotted rate of change for NIGAM in time")


    output_plots <- list(
      plot_result = plot_result,
      plot_rate = plot_rate
    )
  }

  # NIGAM space time
  if (inherits(jags_output, "ni_spline_st") == TRUE) {
    # Dataframes to plot
    output_dataframes <- parameter_estimate(jags_output = jags_output)$output_dataframes
    total_model_df <- output_dataframes$total_model_df
    total_model_rate_df <- output_dataframes$total_model_rate_df
    # Plots
    plot_result <-
      ggplot2::ggplot() +
      ggplot2::geom_rect(data = jags_output$data, ggplot2::aes(
        xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
        ymin = RSL - RSL_err, ymax = RSL + RSL_err, fill = "Uncertainty",
      ), alpha = 0.4) +
      ggplot2::geom_point(
        data = jags_output$data,
        ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.5
      ) +
      ggplot2::geom_line(
        data = total_model_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_df,
        ggplot2::aes(y = RSL, ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = total_model_df,
      #   ggplot2::aes(y = RSL, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
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

    # Plotting Rate of Change for Total component----------
    plot_rate <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = total_model_rate_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_rate_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = total_model_rate_df,
        ggplot2::aes(ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
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


    message("Rate of change completed")

    output_plots <- list(
      plot_result = plot_result,
      plot_rate = plot_rate
    )
  }

  # NIGAM decomposition
  if (inherits(jags_output, "ni_gam_decomp") == TRUE) {
    # Dataframes to plot
    output_dataframes <- parameter_estimate(jags_output = jags_output)$output_dataframes
    total_model_df <- output_dataframes$total_model_df
    total_model_rate_df <-output_dataframes$total_model_rate_df
    # Predicted
    total_model_pred_df <- output_dataframes$mod_output_pred_df
    total_model_pred_rate_df <- output_dataframes$mod_output_pred_deriv_df
    # Regional component
    time_post_component_df <-output_dataframes$time_post_component_df
    time_post_pred_component_df <-output_dataframes$time_post_pred_component_df
    time_post_pred_deriv_component_df <-output_dataframes$time_post_pred_deriv_component_df
    # Linear local component
    g_h_component_post_df <-output_dataframes$g_h_component_post_df
    g_h_component_post_df_pred <-output_dataframes$g_h_component_pred_post_df
    # Non-linear local component
    space_time_component_post_df <-output_dataframes$space_time_component_post_df
    space_time_component_deriv_post_df <-output_dataframes$space_time_component_deriv_post_df
    space_time_component_pred_post_df <-output_dataframes$space_time_component_pred_post_df
    space_time_component_pred_deriv_post_df <-output_dataframes$space_time_component_pred_deriv_post_df


    # Plots
    plot_result <-
      ggplot2::ggplot() +
      ggplot2::geom_rect(data = jags_output$data, ggplot2::aes(
        xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
        ymin = RSL - RSL_err, ymax = RSL + RSL_err, fill = "Uncertainty",
      ), alpha = 0.4) +
      ggplot2::geom_point(
        data = jags_output$data,
        ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.5
      ) +
      ggplot2::geom_line(
        data = total_model_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_df,
        ggplot2::aes(y = RSL, ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = total_model_df,
      #   ggplot2::aes(y = RSL, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
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

    message("Plotted the total model fit for the NIGAM decomposition")

    # Plotting Rate of Change for Total component----------
    plot_rate <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = total_model_rate_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_rate_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = total_model_rate_df,
        ggplot2::aes(ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
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


    message("Rate of change for the total model fit for the NIGAM decomposition")


    # Plots Total Predictions--------
    plot_result_pred <-
      ggplot2::ggplot() +
      ggplot2::geom_rect(data = jags_output$data, ggplot2::aes(
        xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
        ymin = RSL - RSL_err, ymax = RSL + RSL_err, fill = "Uncertainty",
      ), alpha = 0.4) +
      ggplot2::geom_point(
        data = jags_output$data,
        ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.5
      ) +
      ggplot2::geom_line(
        data = total_model_pred_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_pred_df,
        ggplot2::aes(y = RSL, ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      # ggplot2::geom_ribbon(
      #   data = total_model_pred_df,
      #   ggplot2::aes(y = RSL, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
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

    message("Plotted the total model fit for the NIGAM decomposition")

    # Plotting Rate of Change for Total component on prediction grid----------
    plot_rate_pred <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = total_model_pred_rate_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
      ) +
      ggplot2::geom_ribbon(
        data = total_model_pred_rate_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = total_model_pred_rate_df,
        ggplot2::aes(ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
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
    message("Plotted the rate of change for the total model fit for the NIGAM decomposition")

    # Regional Component Plot---------------------------
    regional_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = time_post_component_df,
        ggplot2::aes(x = Age * 1000, y = RSL), colour = "#3b47ad"
      ) +
      ggplot2::geom_ribbon(
        data = time_post_component_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000), fill = "#3b47ad", alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data =time_post_component_df,
        ggplot2::aes(ymin = lwr_50, ymax = upr_50, x = Age * 1000), fill = "#3b47ad", alpha = 0.3
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 15),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        axis.text = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12)
      ) +
      ggplot2::ylab("Sea Level (m)") +
      ggplot2::xlab("Age (CE)")

    message("Regional Component plotted")

    # Derivative of Regional Component Plot---------------------------
    deriv_regional_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = time_deriv_component_post_df,
        ggplot2::aes(x = Age * 1000, y = RSL), colour = "#3b47ad"
      ) +
      ggplot2::geom_ribbon(
        data = time_deriv_component_post_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000), fill = "#3b47ad", alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = time_deriv_component_post_df,
        ggplot2::aes(ymin = lwr_50, ymax = upr_50, x = Age * 1000), fill = "#3b47ad", alpha = 0.3
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 15),
        axis.title = ggplot2::element_text(size = 12, face = "bold"),
        axis.text = ggplot2::element_text(size = 12),
        legend.text = ggplot2::element_text(size = 12)
      ) +
      ggplot2::ylab("Rate of change (mm/yr)") +
      ggplot2::xlab("Age (CE)")

    message("Derivative Regional Component plotted")

    # Regional Component Plot using Prediction---------------------------
    regional_pred_plot<-
      ggplot2::ggplot()+
      ggplot2::geom_line(data=time_post_pred_component_df,
                         ggplot2::aes(x=Age*1000,y=RSL),colour="#3b47ad")+
      ggplot2::geom_ribbon(data=time_post_pred_component_df,
                           ggplot2::aes(ymin=lwr,ymax=upr,x=Age*1000),fill="#3b47ad",alpha=0.2)+
      ggplot2::geom_ribbon(data=time_post_pred_component_df,
                           ggplot2::aes(ymin=lwr_50,ymax=upr_50,x=Age*1000),fill="#3b47ad",alpha=0.3)+
      ggplot2::theme_bw()+
      ggplot2::theme(plot.title = ggplot2::element_text(size=15),
                     axis.title=ggplot2::element_text(size=12,face="bold"),
                     axis.text=ggplot2::element_text(size=12),
                     legend.text=ggplot2::element_text(size=12))+
      ggplot2::ggtitle("Prediction")+
      ggplot2::ylab('Sea Level (m)')+
      ggplot2::xlab('Age (CE)')

    message("Regional Component using Prediction plotted")

    # Derivative of Predicted Regional Component Plot---------------------------
    deriv_pred_regional_plot<-
      ggplot2::ggplot()+
      ggplot2::geom_line(data=time_post_pred_deriv_component_df,
                         ggplot2::aes(x=Age*1000,y=RSL),colour="#3b47ad")+
      ggplot2::geom_ribbon(data=time_post_pred_deriv_component_df,
                           ggplot2::aes(ymin=lwr,ymax=upr,x=Age*1000),fill="#3b47ad",alpha=0.2)+
      ggplot2::geom_ribbon(data=time_post_pred_deriv_component_df,
                           ggplot2::aes(ymin=lwr_50,ymax=upr_50,x=Age*1000),fill="#3b47ad",alpha=0.3)+
      ggplot2::theme_bw()+
      ggplot2::ggtitle("Prediction Rate of Change")+
      ggplot2::theme(plot.title = ggplot2::element_text(size=15),
                     axis.title=ggplot2::element_text(size=12,face="bold"),
                     axis.text=ggplot2::element_text(size=12),
                     legend.text=ggplot2::element_text(size=12))+
      ggplot2::ylab('Rate of Change (mm/yr)')+
      ggplot2::xlab('Age (CE)')

    message("Derivative Predicted Regional Component plotted")


    # Linear Local + Site specific vertical offset---------------------
    g_h_component_df_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = g_h_component_post_df,
        ggplot2::aes(x = Age * 1000, y = RSL), colour = "#5bac06"
      ) +
      ggplot2::geom_ribbon(
        data = g_h_component_post_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000),
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

    message("Linear Local Component plotted")

    # Linear Local + Site specific vertical offset for Prediction---------------------
    g_h_component_df_pred_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = g_h_component_post_df_pred,
        ggplot2::aes(x = Age * 1000, y = RSL), colour = "#5bac06"
      ) +
      ggplot2::geom_ribbon(
        data = g_h_component_post_df_pred,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000),
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

    message("Linear Local Component plotted")

    # Non-Linear Local Component: Spline in Space Time----------------
    local_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = space_time_component_post_df,
        ggplot2::aes(x = Age * 1000, y = RSL), colour = "#ad4c14"
      ) +
      ggplot2::geom_ribbon(
        data = space_time_component_post_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000), fill = "#ad4c14", alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = space_time_component_post_df,
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
    message("Non-Linear Local Component Plot")

    # Non-Linear Local Component: Rate of change for Spline in Space Time----------------
    local_rate_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = space_time_component_deriv_post_df,
        ggplot2::aes(x = Age * 1000, y = RSL), colour = "#ad4c14"
      ) +
      ggplot2::geom_ribbon(
        data = space_time_component_deriv_post_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000), fill = "#ad4c14", alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = space_time_component_deriv_post_df,
        ggplot2::aes(ymin = lwr_50, ymax = upr_50, x = Age * 1000), fill = "#ad4c14", alpha = 0.3
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

    message("Rate of change of Non-Linear Local Component Plot")



    # Non-Linear Local Component: Spline in Space Time with predictions----------------
    local_pred_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = space_time_component_pred_post_df,
        ggplot2::aes(x = Age * 1000, y = RSL), colour = "#ad4c14"
      ) +
      ggplot2::geom_ribbon(
        data = space_time_component_pred_post_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000), fill = "#ad4c14", alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = space_time_component_pred_post_df,
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
    message("Non-Linear Local Component Plot")

    # Non-Linear Local Component: Rate of change for Spline in Space Time prediction----------------
    local_pred_rate_plot <-
      ggplot2::ggplot() +
      ggplot2::geom_line(
        data = space_time_component_pred_deriv_post_df,
        ggplot2::aes(x = Age * 1000, y = RSL), colour = "#ad4c14"
      ) +
      ggplot2::geom_ribbon(
        data = space_time_component_pred_deriv_post_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000), fill = "#ad4c14", alpha = 0.2
      ) +
      ggplot2::geom_ribbon(
        data = space_time_component_pred_deriv_post_df,
        ggplot2::aes(ymin = lwr_50, ymax = upr_50, x = Age * 1000), fill = "#ad4c14", alpha = 0.3
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

    message("Rate of change of Non-Linear Local Component Plot")

    # Separate Components on one plot with CI proxy--------
    all_components_CI_plot <- ggplot2::ggplot() +
      # Local
      ggplot2::geom_line(
        data = space_time_component_post_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = ID)
      ) +
      ggplot2::geom_ribbon(
        data = space_time_component_post_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = ID), alpha = 0.3
      ) +
      # Linear Local Component + site specific vertical offset
      ggplot2::geom_line(
        data = g_h_component_post_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = ID)
      ) +
      ggplot2::geom_ribbon(
        data = g_h_component_post_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = ID), alpha = 0.3
      ) +

      # Regional Component
      ggplot2::geom_line(
        data = time_post_component_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = ID)
      ) +
      ggplot2::geom_ribbon(
        data = time_post_component_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = ID), alpha = 0.3
      ) +
      # Total Model
      ggplot2::geom_line(
        data = total_model_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = ID)
      ) +
      ggplot2::geom_ribbon(
        data = total_model_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = ID), alpha = 0.3
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
      ggplot2::scale_colour_manual(name = "", values = c("#5bac06", "#ad4c14", "#3b47ad", "purple3")) +
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

    cat("All Components plotted")


    # Separate Components on one plot with CI proxy on Prediction grid--------
    all_components_CI_pred_plot <- ggplot2::ggplot() +
      # Local
      ggplot2::geom_line(
        data = space_time_component_pred_post_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = ID)
      ) +
      ggplot2::geom_ribbon(
        data = space_time_component_pred_post_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = ID), alpha = 0.3
      ) +
      # Linear Local Component + site specific vertical offset
      ggplot2::geom_line(
        data = g_h_component_post_df_pred,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = ID)
      ) +
      ggplot2::geom_ribbon(
        data = g_h_component_post_df_pred,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = ID), alpha = 0.3
      ) +

      # Regional Component
      ggplot2::geom_line(
        data = time_post_pred_component_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = ID)
      ) +
      ggplot2::geom_ribbon(
        data = time_post_pred_component_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = ID), alpha = 0.3
      ) +
      # Total Model
      ggplot2::geom_line(
        data = mod_output_pred_df,
        ggplot2::aes(x = Age * 1000, y = RSL, colour = ID)
      ) +
      ggplot2::geom_ribbon(
        data = mod_output_pred_df,
        ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = ID), alpha = 0.3
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
      ggplot2::scale_colour_manual(name = "", values = c("#5bac06", "#ad4c14", "#3b47ad", "purple3")) +
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

    cat("All Components plotted")




    output_plots <- list(
      plot_result = plot_result,
      plot_rate = plot_rate,
      plot_result_pred = plot_result_pred,
      plot_rate_pred = plot_rate_pred,
      regional_plot = regional_plot,
      deriv_regional_plot = deriv_regional_plot,
      regional_pred_plot = regional_pred_plot,
      deriv_pred_regional_plot = deriv_pred_regional_plot,
      g_h_component_df_plot = g_h_component_df_plot,
      g_h_component_df_pred_plot = g_h_component_df_pred_plot,
      local_plot = local_plot,
      local_rate_plot = local_rate_plot,
      local_pred_plot = local_pred_plot,
      local_pred_rate_plot = local_pred_rate_plot,
      all_components_CI_plot = all_components_CI_plot,
      all_components_CI_pred_plot = all_components_CI_pred_plot)
  }
#}
  return(output_plots)
}
