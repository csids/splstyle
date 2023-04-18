#' make_line_excess_graph
#' @param pd a
#' @param x a
#' @param dataVal a
#' @param dataZ a
#' @param dataCIL a
#' @param dataCIU a
#' @param allPoints a
#' @param title a
#' @param pointShift a
#' @param xShift a
#' @param weekNumbers a
#' @param step a
#' @param GetCols a
#' @param legend_position a
#' @import ggplot2
#' @import scales
#' @export
make_line_excess_graph <- function(
                                   pd,
                                   x,
                                   dataVal,
                                   dataZ,
                                   dataCIL = NULL,
                                   dataCIU = NULL,
                                   allPoints = TRUE,
                                   title = NULL,
                                   pointShift = 0,
                                   xShift = 0,
                                   weekNumbers = FALSE,
                                   step = FALSE,
                                   GetCols,
                                   legend_position = "right") {
  pd <- as.data.frame(pd)
  pd$printYear <- format.Date(pd[[x]], "%G")
  pd$printWeek <- format.Date(pd[[x]], "%V")
  pd$printMonth <- format.Date(pd[[x]], "%m")
  pd$printDay <- format.Date(pd[[x]], "%d")
  if (step) {
    pd$xShifted <- pd[[x]] + pointShift
    pd[[x]] <- pd[[x]] + xShift
  } else {
    pd$xShifted <- pd[[x]]
    pd[[x]] <- pd[[x]]
  }
  pd$status <- "Normal"
  pd$status[pd[[dataZ]] > 2] <- "Medium"
  pd$status[pd[[dataZ]] > 4] <- "High"
  includeMedium <- nrow(pd[pd$status == "Medium", ]) > 0
  includeHigh <- nrow(pd[pd$status == "High", ]) > 0

  colours <- NULL
  if (includeHigh) colours <- c(colours, GetCols()[1])
  if (includeMedium) colours <- c(colours, GetCols()[2])

  limits <- range(pd[[x]])
  limitsSize <- max(1, (limits[2] - limits[1]) * 0.005)
  limits[1] <- limits[1] - limitsSize
  limits[2] <- limits[2] + limitsSize

  limitsY <- diff(range(c(pd[[dataCIL]], pd[[dataCIU]])))

  dateBreaks <- "6 months"
  if (as.numeric(difftime(limits[2], limits[1], "days")) / 7 < 52 * 0.25) {
    dateBreaks <- "2 weeks"
  } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 7 < 52 * 0.5) {
    dateBreaks <- "2 weeks"
  } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 7 < 52 * 1) {
    dateBreaks <- "1 month"
  } else if (as.numeric(difftime(limits[2], limits[1], "days")) / 7 < 52 * 2) {
    dateBreaks <- "2 months"
  }

  q <- ggplot(pd, aes_string(x = x))
  if (step) {
    # q <- q + stat_stepribbon(aes_string(ymin = L3, ymax = L4, fill = shQuote("L1")), direction="vh", alpha = 0.4)
    # q <- q + stat_stepribbon(aes_string(ymin = L2, ymax = L3, fill = shQuote("L2")), direction="vh", alpha = 0.4)
    # q <- q + stat_stepribbon(aes_string(ymin = L1, ymax = L2, fill = shQuote("L3")), direction="vh", alpha = 0.4)
    if (!is.null(dataCIL) & !is.null(dataCIU)) q <- q + stat_stepribbon(aes_string(ymin = dataCIL, ymax = dataCIU), fill = "black", direction = "vh", alpha = 0.4)
    q <- q + geom_step(aes_string(y = dataVal), direction = "vh", lwd = 1)
  } else {
    # q <- q + geom_ribbon(aes_string(ymin = L3, ymax = L4, fill = shQuote("L1")), alpha = 0.4)
    # q <- q + geom_ribbon(aes_string(ymin = L2, ymax = L3, fill = shQuote("L2")), alpha = 0.4)
    # q <- q + geom_ribbon(aes_string(ymin = L1, ymax = L2, fill = shQuote("L3")), alpha = 0.4)
    if (!is.null(dataCIL) & !is.null(dataCIU)) q <- q + geom_ribbon(aes_string(ymin = dataCIL, ymax = dataCIU), fill = "black", alpha = 0.4)
    q <- q + geom_line(aes_string(y = dataVal), lwd = 1)
  }

  if (allPoints) {
    q <- q + geom_point(aes_string(x = "xShifted", y = dataVal), size = 4, fill = "black")
  } else {
    if (includeMedium | includeHigh) q <- q + geom_point(aes_string(x = "xShifted", y = dataVal), size = 4, fill = "black", data = pd[pd$status %in% c("Medium", "High"), ])
  }
  if (includeMedium) q <- q + geom_point(aes_string(x = "xShifted", y = dataVal, colour = shQuote("L2")), size = 2, data = pd[pd$status == "Medium", ])
  if (includeHigh) q <- q + geom_point(aes_string(x = "xShifted", y = dataVal, colour = shQuote("L1")), size = 2, data = pd[pd$status == "High", ])
  q <- q + geom_hline(yintercept = 0, colour = "red")
  q <- q + theme_fhi_lines(legend_position = legend_position)

  breaksDF <- pd[pd$printWeek != "", ]
  breaksDF <- DateBreaks(breaksDF, limits, weekNumbers)

  q <- q + scale_x_date("", breaks = breaksDF$xShifted, labels = breaksDF$printLabel)
  q <- q + scale_y_continuous("")
  # q <- q + scale_fill_manual(values=GetCols(),labels=c(
  #  "Betydelig hyere enn forventet",
  #  "Hyere enn forventet",
  #  "Forventet"))
  if (!is.null(colours)) q <- q + scale_colour_manual("", values = colours)
  q <- q + guides(colour = FALSE)
  q <- q + coord_cartesian(xlim = limits, expand = FALSE)
  if (!is.null(title)) q <- q + labs(title = title)
  return(q)
}
