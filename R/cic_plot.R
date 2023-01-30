##' Estimate a changes-in-changes model with multiple periods and cohorts
##'
##' @param object An `ecic` object.
##' @param xlab Alternative x-axis label.
##' @param ylab Alternative y-axis label.
##' @param ylim Define the y-axis limits.
##' @param size Size of the point estimates.
##' @param es_type If an event study was estimated with `ecic`, you can choose the 
##' styple of the ES plot. "aggregated" puts everything in one plot. "for_quantiles"
##' generates one plot for each percentile. "for_periods" generates one plot for each period.
##' @param perc_plot Which percentiles to plot.
##' @param legend_title Change the title of the legend.
##' @return An `ggplot2` object.
##' @export
cic_plot <- function(object, 
                     xlab = NULL, 
                     ylab = "QTE", 
                     ylim = NULL, 
                     size = 2, 
                     es_type = c(NULL, "aggregated", "for_quantiles", "for_periods"),
                     perc_plot = seq(.1, .9, .1),
                     legend_title = "Percentiles") {
  
  es_type    = match.arg(es_type)
  es         = attributes(object)[["ecic_res"]][["es"]]
  periods_es = attributes(object)[["ecic_res"]][["periods_es"]]
  myProbs    = attributes(object)[["ecic_res"]][["myProbs"]]
  
  perc = coefs = es = se = NULL
  
  # Plot the average QTE ----

  if (es == F) {# plot the QTE
    
    if (is.null(xlab)) xlab <- "\n Percentiles"

    ggplot2::ggplot(object, ggplot2::aes(x = perc, y = coefs)) +
      ggplot2::geom_hline(yintercept = 0, col = "grey60") +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = coefs - 1.96 * se, ymax = coefs + 1.96 * se), alpha = .15) +
      ggplot2::geom_line(linetype = "dashed", color = "grey50") +
      ggplot2::geom_point(size = size) +
      ggplot2::coord_cartesian(ylim) +
      ggplot2::theme_minimal() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab)
    
  # Plot an event study for ALL percentiles jointly -----
  } else if (es_type == "aggregated"){
    
    if (is.null(xlab)) xlab <- "\n Months After Treatment"

    myBoot_plot <- subset(do.call(rbind, object), perc %in% perc_plot)
    myBoot_plot$es <- as.factor(myBoot_plot$es)
    myBoot_plot$perc <- as.factor(myBoot_plot$perc)

    ggplot2::ggplot(
      data = myBoot_plot,
      ggplot2::aes(x = es, y = coefs, color = perc, group = perc)
    ) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_line(position = ggplot2::position_dodge(width = 0.3), linetype = "dotted", linewidth = 1.3, alpha = .8) +
      ggplot2::geom_point(size = size, position = ggplot2::position_dodge(width = 0.3)) +
      #ggplot2::geom_errorbar(position = ggplot2::position_dodge(width = 0.3), ggplot2::aes(ymin = coefs - 1.96 * se, ymax = coefs + 1.96 * se), linewidth = 1, width = .4, alpha = .8) +
      ggplot2::geom_ribbon(position = ggplot2::position_dodge(width = 0.3), ggplot2::aes(ymin = coefs - 1.96 * se, ymax = coefs + 1.96 * se), alpha = .15) +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_viridis_d(option = "plasma", begin = .2, end = .8) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::labs(color = legend_title)
    
    # plot every group individually ----
  } else if (es_type == "for_quantiles"){

    plot_title <- "Decile "
    plot_ylab <- "QTE"
    plot_xlab <- "Period"
    plot_data = do.call(rbind, object)

    for (i in 1:length(myProbs)){
      assign(paste0("es", i), 
             ggplot2::ggplot(subset(plot_data, perc == myProbs[i]), ggplot2::aes(x = es, y = coefs, group = 1)) +
               ggplot2::geom_hline(yintercept = 0) +
               ggplot2::geom_ribbon(position = ggplot2::position_dodge(width = 0.3), ggplot2::aes(ymin = coefs - 1.96 * se, ymax = coefs + 1.96 * se), alpha = .2, fill = "deepskyblue2") +
               ggplot2::geom_line(position = ggplot2::position_dodge(width = 0.3), linetype = "dotted", linewidth = 1.1, alpha = .8, color = "darkslateblue") +
               ggplot2::geom_point(size = 4, position = ggplot2::position_dodge(width = 0.3), color = "darkslateblue") +
               ggplot2::theme_minimal() + #ggplot2::coord_cartesian(ylim) +
               ggplot2::xlab(plot_xlab) + ggplot2::ylab(plot_ylab) + 
               ggplot2::ggtitle(paste0(plot_title, myProbs[i]*10))
      )
      
    }
    p <- get("es1")
    for (i in 2:length(myProbs)){
      p <- p + get(paste0("es", i))
    }
    p
    
    # plot every period individually ----
  } else if (es_type == "for_periods"){
    plot_title <- "Period "
    plot_ylab <- "QTE"
    plot_xlab <- "Quantile"
    plot_data = do.call(rbind, object)
    myPeriods <- 1:periods_es
    
    for (i in 1:length(periods_es)){
      assign(paste0("es", i), 
             ggplot2::ggplot(subset(plot_data, es == myPeriods[i]), ggplot2::aes(x = perc, y = coefs, group = 1)) +
               ggplot2::geom_hline(yintercept = 0) +
               ggplot2::geom_ribbon(position = ggplot2::position_dodge(width = 0.3), ggplot2::aes(ymin = coefs - 1.96 * se, ymax = coefs + 1.96 * se), alpha = .2, fill = "deepskyblue2") +
               ggplot2::geom_line(position = ggplot2::position_dodge(width = 0.3), linetype = "dotted", linewidth = 1.1, alpha = .8, color = "darkslateblue") +
               ggplot2::geom_point(size = 4, position = ggplot2::position_dodge(width = 0.3), color = "darkslateblue") +
               ggplot2::theme_minimal() + #ggplot2::coord_cartesian(ylim) +
               ggplot2::xlab(plot_xlab) + ggplot2::ylab(plot_ylab) + 
               ggplot2::ggtitle(paste0(plot_title, myPeriods[i]))
      )
      
    }
    p <- get("es1")
    for (i in 2:length(myPeriods)){
      p <- p + get(paste0("es", i))
    }
    p
  }
}





