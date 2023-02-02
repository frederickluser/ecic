##' @title Plot an extended changes-in-changes model
##' 
##' @description Plots the results of the `ecic` model, either along
##' the percentiles or in an event-study fashion.
##' 
##' @param object An `ecic_table` object.
##' @param es_type If an event study was estimated with `ecic`, you can choose the 
##' style of the ES plot. "aggregated" puts everything in one plot. "for_quantiles"
##' generates one plot for each percentile. "for_periods" generates one plot for each period.
##' @param perc_plot Which percentiles to plot.
##' @param periods_plot Which periods to plot.
##' @param xlab Alternative x-axis label 
##' @param ylab Alternative y-axis label.
##' @param ylim Define the y-axis limits.
##' @param size Size of the point estimates.
##' @param zero_line Add a horizontal line at zero.
##' @param legend_title Change the title of the legend.
##' @return A `ggplot2` object.
##' @importFrom stats sd
##' @export
ecic_plot = function(object, 
                     es_type = c("aggregated", "for_quantiles", "for_periods"),
                     perc_plot = NULL,
                     periods_plot = NULL,        
                     xlab = NULL, 
                     ylab = "QTE \n", 
                     ylim = NULL, 
                     size = 2, 
                     zero_line = FALSE,
                     legend_title = "Percentiles") {
  
  if(class(object)[1] != "ecic_table") stop("`object` needs to be a valid ecic_table object.\n")
  
  es_type    = match.arg(es_type)
  es         = attributes(object)[["ecic_table"]][["es"]]
  periods_es = attributes(object)[["ecic_table"]][["periods_es"]]
  myProbs    = attributes(object)[["ecic_table"]][["myProbs"]]

  perc = coefs = se = NULL
  if (es == FALSE) es_type = NULL
  if(is.null(perc_plot)) perc_plot = myProbs
  
  if (!is.logical(zero_line)) stop("`zero_line` must be logical.")
  if (class(object)[1] != "ecic_table") stop("`object` must be a ecic_table object. Run cic_summary first.")
  if (!is.null(es_type) & (periods_es == 0 | is.na(periods_es))) warning("There is only one period. Average QTEs are plotted.")

  # Plot the average QTE -------------------------------------------------------
  if (es == FALSE) {
    
    if (is.null(xlab)) xlab = "\n Percentiles"

    p = ggplot2::ggplot(
      object, 
      ggplot2::aes(x = perc, y = coefs)
      ) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = coefs - 1.96 * se, ymax = coefs + 1.96 * se), alpha = .15) +
      ggplot2::geom_line(linetype = "dashed", color = "grey50") +
      ggplot2::geom_point(size = size) +
      ggplot2::coord_cartesian(ylim) +
      ggplot2::theme_minimal() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab)
    
    if (zero_line == TRUE) p = p + ggplot2::geom_hline(yintercept = 0, col = "grey60")
    return(p)
    
  # Plot an event study for ALL percentiles jointly ----------------------------
  } else if (es_type == "aggregated"){
    
    if (is.null(xlab)) xlab = "\n Months After Treatment"

    myBoot_plot      = subset(do.call(rbind, object), perc %in% perc_plot)
    myBoot_plot$es   = as.factor(myBoot_plot$es)
    myBoot_plot$perc = as.factor(myBoot_plot$perc)

    p = ggplot2::ggplot(
      data = myBoot_plot,
      ggplot2::aes(x = es, y = coefs, color = perc, group = perc)
      ) +
      ggplot2::geom_line(position = ggplot2::position_dodge(width = 0.3), linetype = "dotted", linewidth = 1.3, alpha = .8) +
      ggplot2::geom_point(size = size, position = ggplot2::position_dodge(width = 0.3)) +
      ggplot2::geom_ribbon(position = ggplot2::position_dodge(width = 0.3), ggplot2::aes(ymin = coefs - 1.96 * se, ymax = coefs + 1.96 * se), alpha = .15) +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_viridis_d(option = "plasma", begin = .2, end = .8) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::labs(color = legend_title)
    
    if (zero_line == TRUE) p = p + ggplot2::geom_hline(yintercept = 0, col = "grey60")
    p
    
    # plot every group individually ----
  } else if (es_type == "for_quantiles"){

    plot_title = "Decile "
    plot_ylab  = "QTE \n"
    plot_xlab  = "\n Period"
    plot_data  = subset(do.call(rbind, object), perc %in% perc_plot)
    
    for (i in seq_along(perc_plot) ){
      assign(paste0("es", i), 
             ggplot2::ggplot(
               subset(plot_data, perc == perc_plot[i]), 
               ggplot2::aes(x = es, y = coefs, group = 1)
               ) +
               ggplot2::geom_ribbon(ggplot2::aes(ymin = coefs - 1.96 * se, ymax = coefs + 1.96 * se), alpha = .2, fill = "deepskyblue2") +
               ggplot2::geom_line(linetype = "dotted", linewidth = 1.1, alpha = .8, color = "darkslateblue") +
               ggplot2::geom_point(size = 4, color = "darkslateblue") +
               ggplot2::theme_minimal() + 
               ggplot2::coord_cartesian(ylim = ylim) +
               ggplot2::xlab(plot_xlab) + 
               ggplot2::ylab(plot_ylab) + 
               ggplot2::ggtitle(paste0(plot_title, perc_plot[i]*10))
      )
      if (zero_line == TRUE) assign(paste0("es", i), get(paste0("es", i)) + ggplot2::geom_hline(yintercept = 0, col = "grey60"))
    }
    
    for (i in seq_along(perc_plot)) {
      if (i == 1) {
        p = list(get("es1"))
      } else {
        p[[length(p)+1]] = get(paste0("es", i))
      }
    }
    return(patchwork::wrap_plots(p))
    
    # plot every period individually ----
  } else if (es_type == "for_periods"){
    
    plot_title = "Period "
    plot_ylab  = "QTE \n"
    plot_xlab  = "\n Quantile"
    myPeriods  =  0:periods_es
    if(is.null(periods_plot)) periods_plot = myPeriods
    plot_data  = subset(do.call(rbind, object), es %in% periods_plot)
    
    for (i in seq_along(periods_plot)){
      assign(paste0("es", i), 
             ggplot2::ggplot(
               subset(plot_data, es == periods_plot[i]), 
               ggplot2::aes(x = perc, y = coefs, group = 1)
               ) +
               ggplot2::geom_ribbon(ggplot2::aes(ymin = coefs - 1.96 * se, ymax = coefs + 1.96 * se), alpha = .2, fill = "deepskyblue2") +
               ggplot2::geom_line(linetype = "dotted", linewidth = 1.1, alpha = .8, color = "darkslateblue") +
               ggplot2::geom_point(size = 4, color = "darkslateblue") +
               ggplot2::theme_minimal() + 
               ggplot2::coord_cartesian(ylim = ylim) +
               ggplot2::xlab(plot_xlab) + 
               ggplot2::ylab(plot_ylab) + 
               ggplot2::ggtitle(paste0(plot_title, periods_plot[i]))
      )
      if (zero_line == TRUE) assign(paste0("es", i), get(paste0("es", i)) + ggplot2::geom_hline(yintercept = 0, col = "grey60"))
    }
    
    for (i in seq_along(periods_plot)){
      if (i == 1){
        p = list(get("es1"))
      } else {
        p[[length(p)+1]] = get(paste0("es", i))
      }
    }
    return(patchwork::wrap_plots(p))
  }
}





