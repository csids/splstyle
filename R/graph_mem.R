#' A treshold chart of the type used for influenza
#'
#' @param data A data.table for a season of influenza
#' @param title the title for the chart
#' @param lang language of labels
#' @param weeks start and end week for the plot
#' @param color_palette which color palette to use (fhi uses standard csstyle, otherwise influensa palette is used)
#' @param legend_control using a legend or text on the chart
#'
#' @import ggplot2
#' @export make_influenza_threshold_chart
make_influenza_threshold_chart <- function(data, title, lang = "NB", weeks = c(40, 20),
                                           color_palette = "fhi", legend_control = "legend") {
  week <- NULL
  low <- NULL
  medium <- NULL
  high <- NULL
  very_high <- NULL
  rate <- NULL
  low_p <- NULL
  high_p <- NULL
  x <- NULL
  y <- NULL
  text <- NULL
  stat_stepribbon <- NULL
  low_n <- NULL
  high_n <- NULL

  first_year <- min(data[, year])
  second_year <- max(data[, year])


  right_side_factor <- 0
  if (legend_control == "text") {
    right_side_factor <- 5
  }

  data <- data[(week >= weeks[1] & year == first_year) | (week <= weeks[2] & year == second_year)]
  if (nrow(data) == 0) {
    data <- rbind(
      data.table(
        week = weeks[1],
        very_high = data[1, very_high],
        high = data[1, high],
        medium = data[1, medium],
        low = data[1, low],
        rate = NA
      ),
      data.table(
        week = weeks[2] + right_side_factor,
        very_high = data[1, very_high],
        high = data[1, high],
        medium = data[1, medium],
        low = data[1, low],
        rate = NA
      )
    )
  }
  last_week <- data[, week][nrow(data)]


  week_levels <- c((weeks[1] - 1):52, 1:(weeks[2] + right_side_factor))

  if ((last_week < (weeks[2] + right_side_factor)) | (first_year == second_year)) {
    data <- rbind(data.table(
      week = weeks[1] - 1,
      very_high = data[1, very_high],
      high = data[1, high],
      medium = data[1, medium],
      low = data[1, low]
    ), data, fill = TRUE)
    data <- rbind(data, data.table(
      week = weeks[2] + right_side_factor,
      very_high = data[1, very_high],
      high = data[1, high],
      medium = data[1, medium],
      low = data[1, low]
    ), fill = TRUE)
  }

  data$week <- factor(data$week, levels = week_levels)
  plot_data <- data[!is.na(week)]

  if (lang == "EN") {
    label_very_low <- "Very low"
    label_low <- "Low"
    label_med <- "Medium"
    label_high <- "High"
    label_very_high <- "Very High"
    ylab <- "% of patients with ILI"
    legend_label <- "Level"
    week_label <- "Week"
  } else if (lang == "NB") {
    label_very_low <- "Sv\u00E6rt lavt"
    label_low <- "Lavt"
    label_med <- "Middels"
    label_high <- "H\u00F8yt"
    label_very_high <- "Sv\u00E6rt h\u00F8yt"
    ylab <- "Andel pasienter med ILS (%)"
    legend_label <- "Niv\u00E5"
    week_label <- "Uke"
  }
  q <- ggplot(plot_data) +
    theme_fhi_basic() +
    geom_ribbon(aes(x = week, ymin = very_high, ymax = very_high * 1.1, fill = "l5", group = 1), alpha = 1) +
    geom_ribbon(aes(x = week, ymin = high, ymax = very_high, fill = "l4", group = 1), alpha = 1) +
    geom_ribbon(aes(x = week, ymin = medium, ymax = high, fill = "l3", group = 1), alpha = 1) +
    geom_ribbon(aes(x = week, ymin = low, ymax = medium, fill = "l2", group = 1), alpha = 1) +
    geom_ribbon(aes(x = week, ymin = 0, ymax = low, fill = "l1", group = 1), alpha = 0.5)


  labels <- c(
    "l1" = label_very_low, "l2" = label_low, "l3" = label_med,
    "l4" = label_high, "l5" = label_very_high
  )

  if (color_palette == "fhi") {
    colors <- vals$pals$map_seq_complete_5
  } else {
    colors <- c("#00586E", "#276B81", "#5793A7", "#43B3CE", "#8DCFE4")
  }

  names(colors) <- c("l5", "l4", "l3", "l2", "l1")
  if ("low_p" %in% colnames(plot_data)) {
    q <- q + geom_ribbon(aes(x = week, ymin = low_p * 100, ymax = high_p * 100, fill = "unc", group = 1), alpha = 0.7)
    labels <- c(labels, "unc" = "Usikkerhet")
    colors <- c(colors, "unc" = "grey")
  }



  q <- q + scale_fill_manual(legend_label,
    labels = labels,
    values = colors
  )

  if (legend_control == "text") {
    end_point <- (52 - weeks[1]) + weeks[2] - 0.8 + right_side_factor
    low <- plot_data[1, low]
    medium <- plot_data[1, medium]
    high <- plot_data[1, high]
    very_high <- plot_data[1, very_high]
    q <- q + theme(legend.position = "none") +
      geom_text(
        data = data.frame(
          text = c(label_very_low, label_low, label_med, label_high, label_very_high),
          x = c(end_point, end_point, end_point, end_point, end_point),
          y = c(low / 2, (medium + low) / 2, (high + medium) / 2, (very_high + high) / 2, very_high * 1.05)
        ),

        aes(x = x, y = y, label = text),
        color = "white",
        fontface = "bold",
        size = 5
      )
  }

  q <- q +
    ggtitle(title) +
    ylab(ylab) +
    scale_y_continuous(
      limits = c(0, max(data[, very_high]) * 1.1),
      expand = expand_scale(mult = c(0, 0))
    ) +

    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_discrete(
      expand = expand_scale(mult = c(0, 0)), drop = FALSE,
      breaks = c(weeks[1]:52, 1:weeks[2])
    ) + theme(
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE
    ) +
    xlab(week_label) +
    theme(
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 13)
    )


  if (sum(!is.na(plot_data[, rate]) > 0)) {
    q <- q + geom_line(data = plot_data[!is.na(rate)], aes(x = week, y = rate, group = 1)) +
      geom_point(data = plot_data[!is.na(rate)], aes(x = week, y = rate, group = 1))
  }
  return(q)
}
