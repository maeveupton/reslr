# # Dataframes for plotting output---------------
# total_model_df <- data.frame(
#   RSL = apply(mu_post_pred, 2, mean),
#   upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
#   lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
#   upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
#   lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
#   Age = jags_output$data_grid$Age,
#   SiteName = jags_output$data_grid$SiteName,
#   ID = "Total Posterior Model"
# )

# if ("linear_rate" %in% colnames(jags_output$data_grid) & "linear_rate_err" %in% colnames(jags_output$data_grid)) {
#   total_model_df <- data.frame(
#     total_model_df,
#     linear_rate = jags_output$data_grid$linear_rate,
#     linear_rate_err = jags_output$data_grid$linear_rate_err
#   )
# }

#output_dataframes <- list(total_model_df = total_model_df)
#output_dataframes <- list(output_dataframes = output_dataframes)


# Output from mcmc------------------------
mu_post_pred <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred
# Dataframes for plotting output---------------
total_model_df <- data.frame(
  RSL = apply(mu_post_pred, 2, mean),
  upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
  lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
  upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
  lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
  Age = jags_output$data_grid$Age,
  SiteName = jags_output$data_grid$SiteName,
  ID = "Total Posterior Model"
)

if ("linear_rate" %in% colnames(jags_output$data_grid) & "linear_rate_err" %in% colnames(jags_output$data_grid)) {
  total_model_df <- data.frame(
    total_model_df,
    linear_rate = jags_output$data_grid$linear_rate,
    linear_rate_err = jags_output$data_grid$linear_rate_err
  )
}

# Output dataframes for plots
output_dataframes <- list(total_model_df = total_model_df)
# plot_result <-
#   ggplot2::ggplot() +
#   ggplot2::geom_rect(data = jags_output$data, ggplot2::aes(
#     xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
#     ymin = RSL - RSL_err, ymax = RSL + RSL_err, fill = "Uncertainty",
#   ), alpha = 0.4) +
#   ggplot2::geom_point(
#     data = jags_output$data,
#     ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.5
#   ) +
#   ggplot2::geom_line(
#     data = total_model_df,
#     ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
#   ) +
#   ggplot2::geom_ribbon(
#     data = total_model_df,
#     ggplot2::aes(y = RSL, ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
#   ) +
#   # ggplot2::geom_ribbon(
#   #   data = total_model_df,
#   #   ggplot2::aes(y = RSL, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
#   # ) +
#   ggplot2::xlab("Age (CE)") +
#   ggplot2::ylab("Relative Sea Level (m)") +
#   ggplot2::theme_bw() +
#   ggplot2::theme(
#     plot.title = ggplot2::element_text(size = 15),
#     axis.title = ggplot2::element_text(size = 12, face = "bold"),
#     axis.text = ggplot2::element_text(size = 12),
#     legend.text = ggplot2::element_text(size = 10)
#   ) +
#   ggplot2::theme(
#     strip.text.x = ggplot2::element_text(size = 10),
#     strip.background = ggplot2::element_rect(fill = c("white"))
#   ) +
#   ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
#   ggplot2::labs(colour = "") +
#   ggplot2::scale_fill_manual("",
#     values = c(
#       "Uncertainty" = ggplot2::alpha("grey", 0.3),
#       "95" = ggplot2::alpha("purple3", 0.2) #
#       # "50" = ggplot2::alpha("purple3", 0.3)
#     ),
#     labels = c(
#       "95% Credible Interval",
#       expression(paste("1-sigma error"))
#       # , "50% Credible Interval"
#     )
#   ) +
#   ggplot2::scale_colour_manual("",
#     values = c("black" = "black", "mean" = "purple3"),
#     labels = c("Data", "Posterior Fit")
#   ) +
#   ggplot2::guides(
#     fill = ggplot2::guide_legend(override.aes = list(
#       alpha = c(0.4, 0.2), # , 0.4),
#       size = 1
#     )),
#     colour = ggplot2::guide_legend(override.aes = list(
#       linetype = c(0, 1),
#       shape = c(16, NA),
#       size = 2
#     ))
#   )


# # Output from mcmc------------------------
# mu_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_y
# mu_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_deriv
# mu_post_pred <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred
# mu_pred_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred_deriv
#
# # Dataframes for plotting output---------------
# total_model_df <- data.frame(
#   RSL = apply(mu_post_pred, 2, mean),
#   upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
#   lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
#   upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
#   lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
#   Age = jags_output$data_grid$Age,
#   SiteName = jags_output$data_grid$SiteName,
#   ID = "Total Posterior Model"
# )
#
# if ("linear_rate" %in% colnames(jags_output$data_grid) & "linear_rate_err" %in% colnames(jags_output$data_grid)) {
#   total_model_df <- data.frame(
#     total_model_df,
#     linear_rate = jags_output$data_grid$linear_rate,
#     linear_rate_err = jags_output$data_grid$linear_rate_err
#   )
# }
# # FINISH this
# # Dataframes for derivative plots from prediction grids------------
# total_model_rate_df <- data.frame(
#   RSL_mod = apply(mu_pred_deriv_post, 2, mean),
#   RSL_mod_upr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.025),
#   RSL_mod_lwr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.975),
#   upr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.25),
#   lwr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.75),
#   jags_output$data_grid$Age,
#   jags_output$data_grid$SiteName,
#   jags_output$data_grid$linear_rate,
#   jags_output$data_grid$linear_rate_err,
#   ID = "Rate of Change of Posterior Model"
# )
# names(total_model_rate_df) <- c(
#   "RSL", "upr", "lwr", "upr_50", "lwr_50",
#   "Age",
#   "SiteName", "linear_rate", "linear_rate_err",
#   "ID"
# )
#
# # Output dataframes for plotting
# output_dataframes <- list(
#   total_model_df = total_model_df,
#   total_model_rate_df = total_model_rate_df
# )
#


# # Output from mcmc------------------------
# mu_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_y
# mu_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_deriv
# mu_post_pred <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred
# mu_pred_deriv_post <- jags_output$noisy_model_run_output$BUGSoutput$sims.list$mu_pred_deriv
#
#
# # Dataframes for plotting output using prediction grid---------------
# total_model_df <- data.frame(
#   RSL_mod = apply(mu_post_pred, 2, mean),
#   RSL_mod_upr = apply(mu_post_pred, 2, stats::quantile, probs = 0.025),
#   RSL_mod_lwr = apply(mu_post_pred, 2, stats::quantile, probs = 0.975),
#   upr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.25),
#   lwr_50 = apply(mu_post_pred, 2, stats::quantile, probs = 0.75),
#   jags_output$data_grid$Age,
#   jags_output$data_grid$SiteName,
#   jags_output$data_grid$linear_rate,
#   jags_output$data_grid$linear_rate_err,
#   ID = "Total Predicted Posterior Model",
#   data_type_id = jags_output$data_grid$data_type_id
# )
# names(total_model_df) <- c(
#   "RSL", "upr", "lwr", "upr_50", "lwr_50",
#   "Age",
#   "SiteName", "linear_rate", "linear_rate_err",
#   "ID", "data_type_id"
# )
#
# # Dataframes for derivative plots from prediction grids------------
# total_model_rate_df <- data.frame(
#   RSL_mod = apply(mu_pred_deriv_post, 2, mean),
#   RSL_mod_upr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.025),
#   RSL_mod_lwr = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.975),
#   upr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.25),
#   lwr_50 = apply(mu_pred_deriv_post, 2, stats::quantile, probs = 0.75),
#   jags_output$data_grid$Age,
#   jags_output$data_grid$SiteName,
#   jags_output$data_grid$linear_rate,
#   jags_output$data_grid$linear_rate_err,
#   ID = "Rate of Change of Posterior Model"
# )
# names(total_model_rate_df) <- c(
#   "RSL", "upr", "lwr", "upr_50", "lwr_50",
#   "Age",
#   "SiteName", "linear_rate", "linear_rate_err",
#   "ID"
# )
#
# # Output data frames for plotting
# output_dataframes <- list(
#   total_model_df = total_model_df,
#   total_model_rate_df = total_model_rate_df
# )

# # Dataframes to plot
# output_dataframes <- parameter_estimate(jags_output = jags_output)$output_dataframes
# total_model_df <- output_dataframes$total_model_df
# total_model_rate_df <- output_dataframes$total_model_rate_df
# # Plots
# plot_result <-
#   ggplot2::ggplot() +
#   ggplot2::geom_rect(data = jags_output$data, ggplot2::aes(
#     xmin = Age * 1000 - Age_err * 1000, xmax = Age * 1000 + Age_err * 1000,
#     ymin = RSL - RSL_err, ymax = RSL + RSL_err, fill = "Uncertainty",
#   ), alpha = 0.4) +
#   ggplot2::geom_point(
#     data = jags_output$data,
#     ggplot2::aes(y = RSL, x = Age * 1000, colour = "black"), size = 0.5
#   ) +
#   ggplot2::geom_line(
#     data = total_model_df,
#     ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
#   ) +
#   ggplot2::geom_ribbon(
#     data = total_model_df,
#     ggplot2::aes(y = RSL, ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
#   ) +
#   # ggplot2::geom_ribbon(
#   #   data = total_model_df,
#   #   ggplot2::aes(y = RSL, ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
#   # ) +
#   ggplot2::xlab("Age (CE)") +
#   ggplot2::ylab("Relative Sea Level (m)") +
#   ggplot2::theme_bw() +
#   ggplot2::theme(
#     plot.title = ggplot2::element_text(size = 15),
#     axis.title = ggplot2::element_text(size = 12, face = "bold"),
#     axis.text = ggplot2::element_text(size = 12),
#     legend.text = ggplot2::element_text(size = 10)
#   ) +
#   ggplot2::theme(
#     strip.text.x = ggplot2::element_text(size = 10),
#     strip.background = ggplot2::element_rect(fill = c("white"))
#   ) +
#   ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
#   ggplot2::labs(colour = "") +
#   ggplot2::scale_fill_manual("",
#     values = c(
#       "95" = ggplot2::alpha("purple3", 0.2),
#       "Uncertainty" = ggplot2::alpha("grey", 0.4)
#       # "50" = ggplot2::alpha("purple3", 0.3)
#     ),
#     labels = c(
#       "95% Credible Interval",
#       expression(paste("1-sigma error"))
#       # , "50% Credible Interval"
#     )
#   ) +
#   ggplot2::scale_colour_manual("",
#     values = c("black" = "black", "mean" = "purple3"),
#     labels = c("Data", "Posterior Fit")
#   ) +
#   ggplot2::guides(
#     fill = ggplot2::guide_legend(override.aes = list(
#       alpha = c(0.4, 0.2), # , 0.4),
#       size = 1
#     )),
#     colour = ggplot2::guide_legend(override.aes = list(
#       linetype = c(0, 1),
#       shape = c(16, NA),
#       size = 2
#     ))
#   ) +
#   ggplot2::facet_wrap(~SiteName)
#
# # Plotting Rate of Change for Total component----------
# plot_rate <-
#   ggplot2::ggplot() +
#   ggplot2::geom_line(
#     data = total_model_rate_df,
#     ggplot2::aes(x = Age * 1000, y = RSL, colour = "mean")
#   ) +
#   ggplot2::geom_ribbon(
#     data = total_model_rate_df,
#     ggplot2::aes(ymin = lwr, ymax = upr, x = Age * 1000, fill = "95"), alpha = 0.2
#   ) +
#   ggplot2::geom_ribbon(
#     data = total_model_rate_df,
#     ggplot2::aes(ymin = lwr_50, ymax = upr_50, x = Age * 1000, fill = "50"), alpha = 0.3
#   ) +
#   ggplot2::theme_bw() +
#   ggplot2::facet_wrap(~SiteName) +
#   ggplot2::ylab("Rate of change (mm/yr)") +
#   ggplot2::theme(
#     plot.title = ggplot2::element_text(size = 22),
#     axis.title = ggplot2::element_text(size = 14, face = "bold"),
#     axis.text = ggplot2::element_text(size = 12),
#     legend.text = ggplot2::element_text(size = 12)
#   ) +
#   ggplot2::theme(
#     strip.text.x = ggplot2::element_text(size = 10),
#     strip.background = ggplot2::element_rect(fill = c("white"))
#   ) +
#   ggplot2::xlab("Age (CE)") +
#   ggplot2::theme(legend.box = "horizontal", legend.position = "bottom") +
#   ggplot2::labs(colour = "") +
#   ggplot2::scale_fill_manual("",
#     values = c(
#       "95" = ggplot2::alpha("purple3", 0.2),
#       "50" = ggplot2::alpha("purple3", 0.3)
#     ),
#     labels = c("95% Credible Interval", "50% Credible Interval")
#   ) +
#   ggplot2::scale_colour_manual("",
#     values = c("mean" = "purple3"),
#     labels = c("Posterior Fit")
#   ) +
#   ggplot2::guides(
#     fill = ggplot2::guide_legend(override.aes = list(
#       alpha = c(0.4, 0.2), # , 0.4),
#       size = 1
#     )),
#     colour = ggplot2::guide_legend(override.aes = list(
#       linetype = c(1),
#       shape = c(NA),
#       size = 2
#     ))
#   )
#
out_bgr <- jags_output$noisy_model_run_output$BUGSoutput$summary[, "Rhat"]
# Print out gelman diagnostics of the output
cat("Gelman diagnostics - these values should all be close to 1.\n")
cat("If not, try a longer run of reslr_mcmc.\n")
print(round(out_bgr, 2))
